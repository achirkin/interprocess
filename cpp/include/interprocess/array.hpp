#include <interprocess/growing_blob.hpp>
#include <interprocess/shared_object_name.hpp>

#include <memory>
#include <utility>

#pragma once

namespace interprocess {

/**
 * Automatically-growing C-style array residing in the shared memory.
 *
 * The array interface of this structure (operator[]) provides no multithreading safety guarantees
 * whatsoever.
 * There's no way to tell the current size of the array as well.
 * Individual processes discover/allocate the shared memory as they try to access it.
 *
 */
template <typename K, typename V>
class array_t {
 public:
  using key_type = K;
  using value_type = V;

  /**
   * The most dangerous fact about the array_t is that growing it at the moment of
   * accessing its element forces remapping of the shared memory and invalidates all references
   * obtained previously.
   * To workaround this, use the `memory_guard` and keep its result alive for the lifetime of the
   * used references.
   */
  class memory_handle;

  array_t(const array_t& other) = delete;
  auto operator=(const array_t& other) = delete;
  array_t(array_t&& other) noexcept
      : blob_{std::exchange(other.blob_, growing_blob_t{})}, size_{std::exchange(other.size_, 0)} {}
  inline auto operator=(array_t&& other) noexcept -> array_t& {
    std::swap(blob_, other.blob_);
    std::swap(size_, other.size_);
    return *this;
  }
  ~array_t() noexcept = default;

  /** Create a new shared array */
  static auto create(const shared_object_name_t& name = {}) noexcept -> array_t {
    return array_t{growing_blob_t{sizeof(value_type) * kInitialSize, name}, kInitialSize};
  }

  /** Look up an existing shared array*/
  static auto lookup(const shared_object_name_t& name) noexcept -> array_t {
    return array_t{growing_blob_t{name}, kInitialSize};
  }

  /** Get the element by its index*/
  [[nodiscard]] inline auto operator[](key_type index) noexcept -> value_type& {
    check_bounds(index);
    return reinterpret_cast<value_type*>(blob_.data())[index];
  }

  /** Get the element by its index */
  [[nodiscard]] inline auto operator[](key_type index) const noexcept -> const value_type& {
    check_bounds(index);
    return reinterpret_cast<const value_type*>(blob_.data())[index];
  }

  /**
   * Get the name of the array.
   * It returns `nullptr` if the underlying storage wasn't created due to an error.
   */
  [[nodiscard]] inline auto name() const noexcept -> const shared_object_name_t* {
    return blob_.name();
  }

  /**
   * Retain the state of the memory mapping (the address of the data) for the lifetime of this
   * handle.
   */
  [[nodiscard]] auto memory_guard() const noexcept
      -> std::shared_ptr<growing_blob_t::memory_handle_t> {
    return blob_.memory_guard();
  }

 private:
  // NB: it may be beneficial for performance to keep a cached pointer to data here.
  mutable growing_blob_t blob_;
  mutable key_type size_;

  // Set the initial size to fill one memory page,
  // because it does not make much sense to allocate less.
  constexpr static key_type kInitialSize = std::max<key_type>(1, 4096 / sizeof(value_type));

  array_t(growing_blob_t&& data, key_type size) noexcept : blob_{data}, size_{size} {};

  inline void check_bounds(key_type index) const noexcept {
    if (index >= size_) [[unlikely]] {  // NOLINT (requires C++20)
      while (index >= size_) {
        size_ <<= 1;
      }
      blob_.grow(sizeof(value_type) * size_);
    }
  }
};

}  // namespace interprocess
