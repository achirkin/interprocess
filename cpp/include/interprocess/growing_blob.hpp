
#include <interprocess/shared_object_name.hpp>

#include <cstddef>
#include <memory>

#pragma once

namespace interprocess {

class growing_blob_impl_t;

class growing_blob_t {
 public:
  /** An opaque handle for retaining the state of the memory mapping at the given time. */
  struct memory_handle_t;
  /** Create an empty blob with no data inside. */
  inline growing_blob_t() noexcept = default;
  /** Allocate a new region of shared memory. */
  inline explicit growing_blob_t(std::size_t initial_size) noexcept
      : growing_blob_t{initial_size, shared_object_name_t{}} {}
  /** Allocate a new region of shared memory with the given name */
  explicit growing_blob_t(std::size_t initial_size, const shared_object_name_t& name) noexcept;
  /** Lookup a region of shared memory by its name. */
  explicit growing_blob_t(const shared_object_name_t& name) noexcept;
  /** Copy constructor */
  growing_blob_t(const growing_blob_t& other) noexcept;
  /** Move constructor */
  inline growing_blob_t(growing_blob_t&& other) noexcept
      : impl_(std::exchange(other.impl_, nullptr)) {}
  /** Copy assignment */
  inline auto operator=(const growing_blob_t& other) noexcept -> growing_blob_t& {
    return *this = growing_blob_t(other);
  }
  /** Move assignment */
  inline auto operator=(growing_blob_t&& other) noexcept -> growing_blob_t& {
    std::swap(impl_, other.impl_);
    return *this;
  }
  /** Destroy a region of shared memory. */
  ~growing_blob_t() noexcept;
  /**
   * After a call to this function, it's illegal to pass / lookup this blob across processes.
   *
   * Depending on the implementation, this function may proof the program against leaking system
   * resources, such as file handles, in case of hard crashes.
   */
  void seal() noexcept;
  /**
   * Increase the size of the shared object.
   * Do nothing, if the requested size is not bigger than the actual size.
   */
  void grow(std::size_t size) noexcept;
  /** Get the global name of this blob, to pass it to other processes for sharing. */
  [[nodiscard]] auto name() const noexcept -> const shared_object_name_t*;
  /**  Pointer to the region of shared memory. */
  [[nodiscard]] auto data() noexcept -> void*;
  [[nodiscard]] auto data() const noexcept -> const void*;

  /**
   * Retain the state of the memory mapping (the address of the data) for the lifetime of this
   * handle.
   */
  [[nodiscard]] auto memory_guard() const noexcept -> std::shared_ptr<memory_handle_t>;

 private:
  growing_blob_impl_t* impl_{nullptr};
};

}  // namespace interprocess
