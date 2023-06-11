#include "memory_map.hpp"
#include "storage_descriptor.hpp"

#include <interprocess/common.hpp>
#include <interprocess/growing_blob.hpp>
#include <interprocess/shared_object_name.hpp>

#include <fcntl.h>     // fallocate
#include <sys/mman.h>  // shm_open, shm_unlink
#include <unistd.h>    // ftruncate, sysconf
#include <atomic>
#include <cerrno>  // errno
#include <new>     // placement new
#include <type_traits>

namespace interprocess {

struct growing_blob_t::memory_handle_t {
  memory_map_t value;
  memory_handle_t(memory_map_t&& x) : value(std::move(x)) {}
};

struct growing_blob_state_t {
  /** Offset of the data map into the file. */
  const std::size_t data_offset;
  /** Current size of the data (shared among the users). */
  std::atomic<std::size_t> data_size;
  /** Counts the number of users of the shared blob; starts with one user. */
  std::atomic<std::int32_t> users{1};

  static_assert(decltype(data_size)::is_always_lock_free&& decltype(users)::is_always_lock_free,
                "These atomics should be lock-free for IPC.");
};
static_assert(std::is_trivially_destructible_v<growing_blob_state_t>,
              "The state may not be destructed cleanly and thus should be "
              "trivially destructible to avoid any resource leaks.");

class growing_blob_impl_t {
 public:
  // Create a new blob
  explicit growing_blob_impl_t(std::size_t size, const shared_object_name_t& name) noexcept
      : name_{name}, size_{size}, descriptor_{name_, true} {
    INTERPROCESS_LOG_DEBUG("Allocating %zu bytes...\n", size);

    if (descriptor_.value() < 0) {
      return;
    }

    auto page_size = std::size_t(sysconf(_SC_PAGE_SIZE));
    auto data_offset = round_up(sizeof(growing_blob_state_t), page_size);
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

    map_state_ = memory_map_t{descriptor_, 0, sizeof(growing_blob_state_t)};
    if (map_state_.data() == nullptr) {
      seal();
      return;
    }
    state_ptr_ = new (map_state_.data()) growing_blob_state_t{data_offset, size};  // NOLINT

    map_data_ = std::make_shared<growing_blob_t::memory_handle_t>(
        memory_map_t{descriptor_, state_ptr_->data_offset, size});
    if (map_data_->value.data() == nullptr) {
      seal();
      return;
    }
  }

  // Lookup an existing blob
  explicit growing_blob_impl_t(const shared_object_name_t& name) noexcept
      : name_{name},
        descriptor_{name, false},
        map_state_{descriptor_, 0, sizeof(growing_blob_state_t)},
        state_ptr_{reinterpret_cast<growing_blob_state_t*>(map_state_.data())} {
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
    map_data_ = std::make_shared<growing_blob_t::memory_handle_t>(
        memory_map_t{descriptor_, state_ptr_->data_offset, size_});
    INTERPROCESS_LOG_DEBUG("Looked-up the blob {%s} placed at %p (size = %zu bytes).\n",
                           name_.c_str(), map_data_->value.data(), size_);
  }

  // Copy the blob within a process
  growing_blob_impl_t(const growing_blob_impl_t& other) noexcept
      : name_{other.name_},
        size_{other.size_},
        descriptor_{other.descriptor_},
        map_state_{descriptor_, 0, sizeof(growing_blob_state_t)},
        state_ptr_{reinterpret_cast<growing_blob_state_t*>(map_state_.data())} {
    if (state_ptr_ != nullptr) {
      state_ptr_->users.fetch_add(1, std::memory_order_seq_cst);
      map_data_ = std::make_shared<growing_blob_t::memory_handle_t>(
          memory_map_t{descriptor_, state_ptr_->data_offset, size_});
    }
  }

  growing_blob_impl_t(growing_blob_impl_t&&) = delete;
  auto operator=(const growing_blob_impl_t& other) -> growing_blob_impl_t& = delete;
  auto operator=(growing_blob_impl_t&& other) -> growing_blob_impl_t& = delete;

  ~growing_blob_impl_t() noexcept {
    if (state_ptr_->users.fetch_sub(1, std::memory_order_seq_cst) <= 1) {
      // Unlinking must happen once across all processes.
      seal();
    }
  }

  void seal() noexcept {
    INTERPROCESS_LOG_DEBUG("Unlinking a shared blob %s.\n", name_.c_str());
    shm_unlink(name_.c_str());
  }

  void grow(std::size_t size) noexcept {
    if (size_ >= size) {
      INTERPROCESS_LOG_DEBUG("The mapping is already big enough (%zu), skipping the grow.\n",
                             size_);
      return;
    }
    auto& tracked_size = state_ptr_->data_size;
    auto loaded_size = tracked_size.load(std::memory_order_seq_cst);
    if (loaded_size < size) {
      // allocate more space in the file
      if (ftruncate(descriptor_.value(), off_t(state_ptr_->data_offset + size)) == 0) {
        // if (fallocate(descriptor_.value(), 0, off_t(state_ptr_->data_offset), off_t(size)) == 0)
        // { In a concurrent environment `fallocate` will only increase the size of the file.
        // Multiple processes then race to update the tracked size atomic, but can only increase
        // its value. Hence the loop:
        while (loaded_size < size) {
          if (tracked_size.compare_exchange_weak(loaded_size, size, std::memory_order_seq_cst)) {
            loaded_size = size;
          }
        }
        INTERPROCESS_LOG_DEBUG(
            "Allocated extra memory in the shared object (total data size = %zu).\n", loaded_size);
      } else {
        INTERPROCESS_LOG_DEBUG("Could not allocate shared memory (fallocate) (%d).\n", errno);
      }
    } else {
      INTERPROCESS_LOG_DEBUG(
          "The shared object is already big enough (%zu), just updating the mapping.\n",
          loaded_size);
    }
    size_ = loaded_size;
    if (map_data_.use_count() == 1) {
      // NB: we can go with munmap/mmap later on systems where mremap is absent
      map_data_->value.remap(size_);
    } else {
      map_data_.reset(new growing_blob_t::memory_handle_t{
          memory_map_t{descriptor_, state_ptr_->data_offset, size_}});
    }
  }

  [[nodiscard]] inline auto name() const noexcept -> const shared_object_name_t& { return name_; }
  [[nodiscard]] inline auto data() const noexcept -> const void* { return map_data_->value.data(); }
  [[nodiscard]] inline auto data() noexcept -> void* { return map_data_->value.data(); }
  [[nodiscard]] inline auto memory_guard() const noexcept
      -> std::shared_ptr<growing_blob_t::memory_handle_t> {
    return map_data_;
  }

 private:
  shared_object_name_t name_;
  std::size_t size_;
  storage_descriptor_t descriptor_;
  memory_map_t map_state_;
  growing_blob_state_t* state_ptr_{nullptr};
  std::shared_ptr<growing_blob_t::memory_handle_t> map_data_;
};

growing_blob_t::growing_blob_t(std::size_t size, const shared_object_name_t& name) noexcept
    : impl_{new growing_blob_impl_t{size, name}} {}

growing_blob_t::growing_blob_t(const shared_object_name_t& name) noexcept
    : impl_{new growing_blob_impl_t{name}} {}

growing_blob_t::growing_blob_t(const growing_blob_t& other) noexcept
    : impl_{other.impl_ != nullptr ? new growing_blob_impl_t{*(other.impl_)} : nullptr} {}

growing_blob_t::~growing_blob_t() noexcept {
  // NB: impl_ can be nullptr, but deleting a nullptr has no effect
  delete impl_;
}

void growing_blob_t::seal() noexcept {
  if (impl_ != nullptr) {
    impl_->seal();
  }
}

void growing_blob_t::grow(std::size_t size) noexcept {
  if (impl_ != nullptr) {
    impl_->grow(size);
  }
}

[[nodiscard]] auto growing_blob_t::name() const noexcept -> const shared_object_name_t* {
  return impl_ != nullptr ? &impl_->name() : nullptr;
}

[[nodiscard]] auto growing_blob_t::data() noexcept -> void* {
  return impl_ != nullptr ? impl_->data() : nullptr;
}
[[nodiscard]] auto growing_blob_t::data() const noexcept -> const void* {
  return impl_ != nullptr ? impl_->data() : nullptr;
}

[[nodiscard]] auto growing_blob_t::memory_guard() const noexcept
    -> std::shared_ptr<growing_blob_t::memory_handle_t> {
  return impl_ != nullptr ? impl_->memory_guard() : nullptr;
}

}  // namespace interprocess