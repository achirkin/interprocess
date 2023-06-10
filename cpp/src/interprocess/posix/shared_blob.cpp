#include "memory_map.hpp"
#include "storage_descriptor.hpp"

#include <interprocess/common.hpp>
#include <interprocess/shared_blob.hpp>
#include <interprocess/shared_object_name.hpp>

#include <sys/mman.h>  // shm_open, shm_unlink
#include <unistd.h>    // ftruncate, sysconf
#include <atomic>
#include <cerrno>  // errno
#include <new>     // placement new
#include <type_traits>

namespace interprocess {

// /** Get the pointer into the mapping at the given offset (in bytes). */
// auto data_at(std::ptrdiff_t offset_bytes) noexcept -> void* {
//   return data == nullptr
//              ? nullptr
//              : reinterpret_cast<void*>(reinterpret_cast<std::uint8_t*>(data) + offset_bytes);
// }

struct shared_blob_state_t {
  /** Offset of the data map into the file. */
  const std::size_t data_offset;
  /** Current size of the data (shared among the users). */
  std::atomic<std::size_t> data_size;
  /** Counts the number of users of the shared blob; starts with one user. */
  std::atomic<int32_t> users{1};

  static_assert(decltype(data_size)::is_always_lock_free&& decltype(users)::is_always_lock_free,
                "These atomics should be lock-free for IPC.");
};
static_assert(std::is_trivially_destructible_v<shared_blob_state_t>,
              "The state may not be destructed cleanly and thus should be "
              "trivially destructible to avoid any resource leaks.");

class shared_blob_impl_t {
 public:
  // Create a new blob
  explicit shared_blob_impl_t(std::size_t size, const shared_object_name_t& name) noexcept
      : name_{name}, size_{size}, descriptor_{name_, true} {
    INTERPROCESS_LOG_DEBUG("Allocating %zu bytes...\n", size);

    if (descriptor_.value() < 0) {
      return;
    }

    auto page_size = std::size_t(sysconf(_SC_PAGE_SIZE));
    auto data_offset = round_up(sizeof(shared_blob_state_t), page_size);
    auto total_size = data_offset + size;

    if (ftruncate(descriptor_.value(), off_t(total_size)) != 0) {
      // if (fallocate(descriptor_.value(), 0, off_t{0}, off_t(total_size)) != 0) {
      // NB: int fallocate(int fd, int mode, off_t offset, off_t len)
      //     is non-portable, Linux-only function.
      //     'mode == 0' allows MT-safe growing of the file without shrinking it,
      //     which is the best fit for my use-case.
      INTERPROCESS_LOG_DEBUG("Could not allocate shared memory (fallocate) (%d).\n", errno);
      seal();
      return;
    }
    INTERPROCESS_LOG_DEBUG("Creating a shared blob {%s}; filesystem-reported size: %zu\n",
                           name_.c_str(), descriptor_.get_size());

    map_state_ = memory_map_t{descriptor_, 0, sizeof(shared_blob_state_t)};
    if (map_state_.data() == nullptr) {
      seal();
      return;
    }
    state_ptr_ = new (map_state_.data()) shared_blob_state_t{data_offset, size};  // NOLINT

    map_data_ = memory_map_t{descriptor_, state_ptr_->data_offset, size};
    if (map_data_.data() == nullptr) {
      seal();
      return;
    }
  }

  // Lookup an existing blob
  explicit shared_blob_impl_t(const shared_object_name_t& name) noexcept
      : name_{name},
        descriptor_{name, false},
        map_state_{descriptor_, 0, sizeof(shared_blob_state_t)},
        state_ptr_{reinterpret_cast<shared_blob_state_t*>(map_state_.data())} {
    if (state_ptr_ == nullptr) {
      descriptor_ = storage_descriptor_t{};
      map_state_ = memory_map_t{};
      return;
    }
    // update the state and check if we haven't already sealed the shared object
    if (state_ptr_->users.fetch_add(1, std::memory_order_seq_cst) <= 0) {
      INTERPROCESS_LOG_DEBUG(
          "Found that the blob was already destroyed when updating the total "
          "users counter.\n");
      descriptor_ = storage_descriptor_t{};
      map_state_ = memory_map_t{};
      state_ptr_ = nullptr;
      return;
    }
    size_ = state_ptr_->data_size.load();
    map_data_ = memory_map_t{descriptor_, state_ptr_->data_offset, size_};
  }

  // Copy the blob within a process
  shared_blob_impl_t(const shared_blob_impl_t& other) noexcept
      : name_{other.name_},
        size_{other.size_},
        descriptor_{other.descriptor_},
        map_state_{descriptor_, 0, sizeof(shared_blob_state_t)},
        state_ptr_{reinterpret_cast<shared_blob_state_t*>(map_state_.data())} {
    if (state_ptr_ != nullptr) {
      state_ptr_->users.fetch_add(1, std::memory_order_seq_cst);
      map_data_ = memory_map_t{descriptor_, state_ptr_->data_offset, size_};
    }
  }

  shared_blob_impl_t(shared_blob_impl_t&&) = delete;
  auto operator=(const shared_blob_impl_t& other) -> shared_blob_impl_t& = delete;
  auto operator=(shared_blob_impl_t&& other) -> shared_blob_impl_t& = delete;

  ~shared_blob_impl_t() noexcept {
    if (state_ptr_->users.fetch_sub(1, std::memory_order_seq_cst) <= 1) {
      // Unlinking must happen once across all processes.
      seal();
    }
  }

  void seal() noexcept {
    INTERPROCESS_LOG_DEBUG("Unlinking a shared blob %s.\n", name_.c_str());
    shm_unlink(name_.c_str());
  }

  [[nodiscard]] inline auto name() const noexcept -> const shared_object_name_t& { return name_; }
  [[nodiscard]] inline auto data() const noexcept -> const void* { return map_data_.data(); }
  [[nodiscard]] inline auto data() noexcept -> void* { return map_data_.data(); }

 private:
  shared_object_name_t name_;
  std::size_t size_;
  storage_descriptor_t descriptor_;
  memory_map_t map_state_;
  memory_map_t map_data_;
  shared_blob_state_t* state_ptr_{nullptr};
};

shared_blob_t::shared_blob_t(std::size_t size, const shared_object_name_t& name) noexcept
    : impl_{new shared_blob_impl_t{size, name}} {}

shared_blob_t::shared_blob_t(const shared_object_name_t& name) noexcept
    : impl_{new shared_blob_impl_t{name}} {}

shared_blob_t::shared_blob_t(const shared_blob_t& other) noexcept
    : impl_{other.impl_ != nullptr ? new shared_blob_impl_t{*(other.impl_)} : nullptr} {}

shared_blob_t::~shared_blob_t() noexcept {
  // NB: impl_ can be nullptr, but deleting a nullptr has no effect
  delete impl_;
}

void shared_blob_t::seal() noexcept {
  if (impl_ != nullptr) {
    impl_->seal();
  }
}

[[nodiscard]] auto shared_blob_t::name() const noexcept -> const shared_object_name_t* {
  return impl_ != nullptr ? &impl_->name() : nullptr;
}

[[nodiscard]] auto shared_blob_t::data() noexcept -> void* {
  return impl_ != nullptr ? impl_->data() : nullptr;
}
[[nodiscard]] auto shared_blob_t::data() const noexcept -> const void* {
  return impl_ != nullptr ? impl_->data() : nullptr;
}

}  // namespace interprocess
