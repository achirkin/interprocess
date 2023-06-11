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

memory_map_t::memory_map_t(memory_map_t&& other) noexcept
    : size_{std::exchange(other.size_, 0)}, data_{std::exchange(other.data_, nullptr)} {}

auto memory_map_t::operator=(memory_map_t&& other) noexcept -> memory_map_t& {
  std::swap(size_, other.size_);
  std::swap(data_, other.data_);
  return *this;
}

memory_map_t::~memory_map_t() noexcept {
  if (data_ != nullptr) {
    munmap(data_, size_);
  }
}

void memory_map_t::remap(std::size_t new_size) noexcept {
  auto new_addr = mremap(data_, size_, new_size, MREMAP_MAYMOVE);  // NOLINT
  if (new_addr == MAP_FAILED) {
    INTERPROCESS_LOG_DEBUG("Could not remap shared memory (mremap) (%d).\n", errno);
  } else {
    data_ = new_addr;
    size_ = new_size;
  }
}

}  // namespace interprocess