#include "storage_descriptor.hpp"

#include <interprocess/common.hpp>

#include <fcntl.h>     // O_* constants
#include <sys/mman.h>  // shm_open
#include <sys/stat.h>  // mman mode constants, fstat
#include <unistd.h>    // close, dup, dup2
#include <cerrno>      // errno

namespace interprocess {

void close_fd(int& fd) noexcept {
  if (fd >= 0) {
    if (close(fd) < 0) {
      INTERPROCESS_LOG_DEBUG("Could not close a file descriptor (%d).\n", errno);
    }
    fd = -1;
  }
}

void dup_fd(int& old_fd, int new_fd) noexcept {
  int r = 0;
  if (old_fd >= 0) {
    // need to close the old descriptor if successful
    if (new_fd >= 0) {
      r = dup2(new_fd, old_fd);
    } else {
      close_fd(old_fd);
    }
  } else {
    // orig descriptor is empty, no need to cleanup.
    if (new_fd >= 0) {
      r = dup(new_fd);
      old_fd = r;
    }
  }
  if (r < 0) {
    INTERPROCESS_LOG_DEBUG("Could not duplicate the storage descriptor (%d).\n", errno);
  }
}

storage_descriptor_t::storage_descriptor_t(const shared_object_name_t& name, bool create) noexcept
    : value_{shm_open(reinterpret_cast<const char*>(name.value.data()),
                      create ? O_CREAT | O_RDWR : O_RDWR, S_IRUSR | S_IWUSR)} {
  if (value_ < 0) {
    INTERPROCESS_LOG_DEBUG("Could not open shared memory segment (%d).\n", errno);
  }
}

storage_descriptor_t::storage_descriptor_t(const storage_descriptor_t& other) noexcept {
  dup_fd(value_, other.value_);
}

storage_descriptor_t::storage_descriptor_t(storage_descriptor_t&& other) noexcept
    : value_{std::exchange(other.value_, -1)} {}

auto storage_descriptor_t::operator=(const storage_descriptor_t& other) noexcept
    -> storage_descriptor_t& {
  dup_fd(value_, other.value_);
  return *this;
}

auto storage_descriptor_t::operator=(storage_descriptor_t&& other) noexcept
    -> storage_descriptor_t& {
  std::swap(value_, other.value_);
  return *this;
}

storage_descriptor_t::~storage_descriptor_t() noexcept { close_fd(value_); }

[[nodiscard]] auto storage_descriptor_t::get_size() const noexcept -> std::size_t {
  if (value_ < 0) {
    return 0;
  }
  struct stat buf;  // NOLINT
  if (fstat(value_, &buf) != 0) {
    INTERPROCESS_LOG_DEBUG("Could not get the shared blob size (%d).\n", errno);
    return 0;
  }
  return std::size_t(std::max<off_t>(buf.st_size, 0));
}

}  // namespace interprocess