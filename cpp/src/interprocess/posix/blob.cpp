#include "memory_map.hpp"
#include "storage_descriptor.hpp"

#include <interprocess/blob.hpp>
#include <interprocess/common.hpp>
#include <interprocess/shared_object_name.hpp>

#include <fcntl.h>     // fallocate
#include <sys/mman.h>  // shm_open, shm_unlink
#include <unistd.h>    // ftruncate, sysconf
#include <atomic>
#include <cerrno>  // errno
#include <new>     // placement new
#include <optional>
#include <type_traits>

namespace interprocess {

struct blob_state_t {
  /** Offset of the data map into the file. */
  const std::size_t data_offset;
  /** Maximum allowed size of the data. */
  const std::size_t max_size;
  /** Current size of the data (shared among the users). */
  std::atomic<std::size_t> data_size;
  /** Counts the number of users of the shared blob; starts with one user. */
  std::atomic<std::int32_t> users{1};

  static_assert(decltype(data_size)::is_always_lock_free && decltype(users)::is_always_lock_free,
                "These atomics should be lock-free for IPC.");
};
static_assert(std::is_trivially_destructible_v<blob_state_t>,
              "The state may not be destructed cleanly and thus should be "
              "trivially destructible to avoid any resource leaks.");

class blob_impl_t {
 public:
  // Create a new blob
  explicit blob_impl_t(std::optional<std::size_t> initial_size, std::optional<std::size_t> max_size,
                       const shared_object_name_t& name) noexcept
      : name_{name}, descriptor_{name_, true} {
    auto page_size = std::size_t(sysconf(_SC_PAGE_SIZE));
    size_ = initial_size.value_or(std::min(max_size.value_or(page_size), page_size));
    if (!max_size.has_value()) {
      // NB: also check windows version https://stackoverflow.com/a/2513561
      long phys_pages = sysconf(_SC_PHYS_PAGES);
      if (phys_pages == -1) {
        INTERPROCESS_LOG_DEBUG(
            "Failed to estimate the max blob size (sysconf(_SC_PAGE_SIZE)) (%d)\n", errno);
        phys_pages = 1024 * 1024;  // NOLINT
      }
      max_size.emplace(page_size * size_t(phys_pages));
    }
    INTERPROCESS_LOG_DEBUG("Allocating %zu bytes (max = %zu)...\n", size_, *max_size);

    if (descriptor_.value() < 0) {
      return;
    }

    auto data_offset = round_up(sizeof(blob_state_t), page_size);
    auto total_size = data_offset + size_;

    if (ftruncate(descriptor_.value(), off_t(total_size)) != 0) {
      INTERPROCESS_LOG_DEBUG("Could not allocate shared memory (fallocate) (%d).\n", errno);
      seal();
      return;
    }
    INTERPROCESS_LOG_DEBUG("Creating a shared blob {%s}; filesystem-reported size: %zu\n",
                           name_.c_str(), descriptor_.get_size());

    map_state_ = memory_map_t{descriptor_, 0, sizeof(blob_state_t)};
    if (map_state_.data() == nullptr) {
      seal();
      return;
    }
    state_ptr_ = new (map_state_.data()) blob_state_t{data_offset, *max_size, size_};  // NOLINT

    map_data_ = memory_map_t{descriptor_, state_ptr_->data_offset, size_, state_ptr_->max_size};
    if (map_data_.data() == nullptr) {
      seal();
      return;
    }
  }

  // Lookup an existing blob
  explicit blob_impl_t(const shared_object_name_t& name) noexcept
      : name_{name},
        descriptor_{name, false},
        map_state_{descriptor_, 0, sizeof(blob_state_t)},
        state_ptr_{reinterpret_cast<blob_state_t*>(map_state_.data())} {
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
    map_data_ = memory_map_t{descriptor_, state_ptr_->data_offset, size_, state_ptr_->max_size};
    INTERPROCESS_LOG_DEBUG("Looked-up the blob {%s} placed at %p (size = %zu bytes).\n",
                           name_.c_str(), map_data_.data(), size_);
  }

  // Copy the blob within a process
  blob_impl_t(const blob_impl_t& other) noexcept
      : name_{other.name_},
        size_{other.size_},
        descriptor_{other.descriptor_},
        map_state_{descriptor_, 0, sizeof(blob_state_t)},
        state_ptr_{reinterpret_cast<blob_state_t*>(map_state_.data())} {
    if (state_ptr_ != nullptr) {
      state_ptr_->users.fetch_add(1, std::memory_order_seq_cst);
      map_data_ = memory_map_t{descriptor_, state_ptr_->data_offset, size_, state_ptr_->max_size};
    }
  }

  blob_impl_t(blob_impl_t&&) = delete;
  auto operator=(const blob_impl_t& other) -> blob_impl_t& = delete;
  auto operator=(blob_impl_t&& other) -> blob_impl_t& = delete;

  ~blob_impl_t() noexcept {
    if (state_ptr_->users.fetch_sub(1, std::memory_order_seq_cst) <= 1) {
      // Unlinking must happen once across all processes.
      seal();
    }
  }

  void seal() noexcept {
    INTERPROCESS_LOG_DEBUG("Unlinking a shared blob %s.\n", name_.c_str());
    shm_unlink(name_.c_str());
  }

  auto grow(std::size_t size) noexcept -> std::size_t {
    if (state_ptr_->max_size < size) {
      INTERPROCESS_LOG_DEBUG("Reached the limit of the blob size (%zu > %zu).\n", size,
                             state_ptr_->max_size);
      size = state_ptr_->max_size;
    }
    if (size_ >= size) {
      INTERPROCESS_LOG_DEBUG("The mapping is already big enough (%zu), skipping the grow.\n",
                             size_);
      return size_;
    }
    auto& tracked_size = state_ptr_->data_size;
    auto loaded_size = tracked_size.load(std::memory_order_seq_cst);
    if (loaded_size < size) {
      // allocate more space in the file
      if (fallocate(descriptor_.value(), 0, off_t(state_ptr_->data_offset), off_t(size)) == 0) {
        // NB: In a concurrent environment `fallocate` will only increase the size of the file.
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
    // size may still change!
    size_ = map_data_.remap(descriptor_, state_ptr_->data_offset, size_);
    return size_;
  }

  [[nodiscard]] inline auto name() const noexcept -> const shared_object_name_t& { return name_; }
  [[nodiscard]] inline auto data() const noexcept -> const void* { return map_data_.data(); }
  [[nodiscard]] inline auto data() noexcept -> void* { return map_data_.data(); }

 private:
  shared_object_name_t name_;
  std::size_t size_;
  storage_descriptor_t descriptor_;
  memory_map_t map_state_;
  blob_state_t* state_ptr_{nullptr};
  memory_map_t map_data_;
};

auto blob_t::create(std::optional<std::size_t> initial_size, std::optional<std::size_t> max_size,
                    const shared_object_name_t& name) noexcept -> blob_t {
  return blob_t{new blob_impl_t{initial_size, max_size, name}};
}

auto blob_t::lookup(const shared_object_name_t& name) noexcept -> blob_t {
  return blob_t{new blob_impl_t{name}};
}

blob_t::blob_t(const blob_t& other) noexcept
    : impl_{other.impl_ != nullptr ? new blob_impl_t{*(other.impl_)} : nullptr} {}

blob_t::~blob_t() noexcept {
  // NB: impl_ can be nullptr, but deleting a nullptr has no effect
  delete impl_;
}

void blob_t::seal() noexcept {
  if (impl_ != nullptr) {
    impl_->seal();
  }
}

auto blob_t::grow(std::size_t size) noexcept -> std::size_t {
  if (impl_ != nullptr) {
    return impl_->grow(size);
  }
  return 0;
}

[[nodiscard]] auto blob_t::name() const noexcept -> const shared_object_name_t* {
  return impl_ != nullptr ? &impl_->name() : nullptr;
}

[[nodiscard]] auto blob_t::data() noexcept -> void* {
  return impl_ != nullptr ? impl_->data() : nullptr;
}
[[nodiscard]] auto blob_t::data() const noexcept -> const void* {
  return impl_ != nullptr ? impl_->data() : nullptr;
}

}  // namespace interprocess
