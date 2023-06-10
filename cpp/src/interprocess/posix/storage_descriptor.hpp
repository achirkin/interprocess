
#include <interprocess/shared_object_name.hpp>

#pragma once

namespace interprocess {

/**
 * Wraps an OS-specific file descriptor into a C++-friendly interface.
 *
 * Note:
 *   It's not safe to use the same descriptor in multiple threads.
 *   However, you can copy a storage descriptor, which would duplicate the OS handle,
 *   this entails all the same safety guarantees as multiple OS handles pointing to
 *   the same file.
 */
struct storage_descriptor_t {
  /** Make an invalid file descriptor */
  inline storage_descriptor_t() noexcept = default;
  /** Create or open a new descriptor */
  storage_descriptor_t(const shared_object_name_t& name, bool create) noexcept;
  /** Duplicate the descriptor */
  storage_descriptor_t(const storage_descriptor_t& other) noexcept;
  /** Move the descriptor */
  storage_descriptor_t(storage_descriptor_t&& other) noexcept;
  /** Duplicate the descriptor */
  auto operator=(const storage_descriptor_t& other) noexcept -> storage_descriptor_t&;
  /** Move the descriptor */
  auto operator=(storage_descriptor_t&& other) noexcept -> storage_descriptor_t&;
  /** Close the descriptor */
  ~storage_descriptor_t() noexcept;
  /** Query the file size from the OS. */
  [[nodiscard]] auto get_size() const noexcept -> std::size_t;
  /** Get the actual OS-specific file descriptor */
  [[nodiscard]] inline auto value() const noexcept -> int { return value_; }

 private:
  int value_ = -1;
};

}  // namespace interprocess