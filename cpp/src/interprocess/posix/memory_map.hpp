#include "storage_descriptor.hpp"

#pragma once

namespace interprocess {

/** Wrap the OS calls map/unmap in a C++-friendly interface. */
struct memory_map_t {
  inline memory_map_t() noexcept = default;
  /**
   * Map a virtual memory range given by the offset and the size onto a storage.
   * Both offset and size are given in bytes and must satisfy the requirements of the OS function
   * mmap.
   */
  memory_map_t(const storage_descriptor_t& descriptor, std::size_t offset,
               std::size_t size) noexcept;
  /**
   * First reserve a memory (PROT_NONE / MAP_PRIVE | MAP_ANONYMOUS | MAP_NORESERVE), then mmap part
   * of it.
   */
  memory_map_t(const storage_descriptor_t& descriptor, std::size_t offset, std::size_t size,
               std::size_t reserved_size) noexcept;
  memory_map_t(const memory_map_t&) = delete;  // no copies allowed
  memory_map_t(memory_map_t&& other) noexcept;
  auto operator=(const memory_map_t&) noexcept -> memory_map_t& = delete;  // no copies allowed
  auto operator=(memory_map_t&& other) noexcept -> memory_map_t&;
  ~memory_map_t() noexcept;

  /** Size of the mapping in bytes */
  [[nodiscard]] inline auto size() const noexcept -> std::size_t { return size_; }
  /** The reserved address range */
  [[nodiscard]] inline auto reserved_size() const noexcept -> std::size_t {
    return std::max(size_, reserved_size_);
  }
  /** A pointer to the mapped data */
  [[nodiscard]] inline auto data() const noexcept -> const void* { return data_; }
  /** A pointer to the mapped data */
  [[nodiscard]] inline auto data() noexcept -> void* { return data_; }
  /**
   * Change the size of the mapping.
   * The descriptor and the offset must be the same as provided in the constructor (not checked).
   *
   * Returns the new size, which may be different from the given size due to reservation limit or
   * errors.
   */
  auto remap(const storage_descriptor_t& descriptor, std::size_t offset, std::size_t size) noexcept
      -> std::size_t;

 private:
  std::size_t reserved_size_{0};
  std::size_t size_{0};
  void* data_{nullptr};
};

}  // namespace interprocess