#include "memory_map.hpp"
#include "storage_descriptor.hpp"

#include <interprocess/common.hpp>

#include <sys/mman.h>  // mmap, munmap
#include <cerrno>      // errno

namespace interprocess {

memory_map_t::memory_map_t(const storage_descriptor_t& descriptor, std::size_t offset,
                           std::size_t size) noexcept
    : size_(size),
      data_(descriptor.value() < 0 || size == 0 ? nullptr
                                                : mmap(nullptr, size, PROT_READ | PROT_WRITE,
                                                       MAP_SHARED, descriptor.value(), offset)) {
  if (data_ == MAP_FAILED) {
    INTERPROCESS_LOG_DEBUG("Could not map shared memory (mmap) (%d).\n", errno);
    size_ = 0;
    data_ = nullptr;
  }
}

memory_map_t::memory_map_t(const storage_descriptor_t& descriptor, std::size_t offset,
                           std::size_t size, std::size_t reserved_size) noexcept
    : reserved_size_(reserved_size),
      size_(std::min(reserved_size, size)),
      data_(reserved_size == 0 ? nullptr
                               : mmap(nullptr, reserved_size, PROT_NONE,
                                      MAP_PRIVATE | MAP_ANONYMOUS | MAP_NORESERVE, -1, 0)) {
  if (data_ == MAP_FAILED) {
    INTERPROCESS_LOG_DEBUG("Could not reserve shared memory (mmap) (%d).\n", errno);
    reserved_size_ = 0;
    size_ = 0;
    data_ = nullptr;
    return;
  }
  if (data_ == nullptr || size_ == 0 || descriptor.value() < 0) {
    size_ = 0;
    return;
  }
  if (mmap(data_, size_, PROT_READ | PROT_WRITE, MAP_SHARED | MAP_FIXED, descriptor.value(),
           offset) == MAP_FAILED) {
    INTERPROCESS_LOG_DEBUG("Could not map shared memory (mmap) (%d).\n", errno);
    size_ = 0;
    return;
  }
}

memory_map_t::memory_map_t(memory_map_t&& other) noexcept
    : reserved_size_{std::exchange(other.reserved_size_, 0)},
      size_{std::exchange(other.size_, 0)},
      data_{std::exchange(other.data_, nullptr)} {}

auto memory_map_t::operator=(memory_map_t&& other) noexcept -> memory_map_t& {
  std::swap(reserved_size_, other.reserved_size_);
  std::swap(size_, other.size_);
  std::swap(data_, other.data_);
  return *this;
}

memory_map_t::~memory_map_t() noexcept {
  if (data_ != nullptr) {
    munmap(data_, std::max(size_, reserved_size_));
  }
}

auto memory_map_t::remap(const storage_descriptor_t& descriptor, std::size_t offset,
                         std::size_t size) noexcept -> std::size_t {
  // TODO: proper zero/smaller size handling
  if (reserved_size_ > 0) {  // Mapping address is fixed
    auto new_size = std::min(size, reserved_size_);
    if (mmap(data_, new_size, PROT_READ | PROT_WRITE, MAP_SHARED | MAP_FIXED, descriptor.value(),
             offset) == MAP_FAILED) {
      INTERPROCESS_LOG_DEBUG("Could not remap shared memory (mremap) (%d).\n", errno);
    } else {
      size_ = new_size;
    }
  } else {  // Allowed to change the address
#ifdef _GNU_SOURCE
    auto new_addr = mremap(data_, size_, size, MREMAP_MAYMOVE);  // NOLINT
    if (new_addr == MAP_FAILED) {
      INTERPROCESS_LOG_DEBUG("Could not remap shared memory (mremap) (%d).\n", errno);
    } else {
      data_ = new_addr;
      size_ = size;
    }
#else
    memory_map_t other{descriptor, offset, size};
    if (other.size > 0) {
      std::swap(size_, other.size_);
      std::swap(data_, other.data_);
    }
#endif
  }
  return size_;
}

}  // namespace interprocess