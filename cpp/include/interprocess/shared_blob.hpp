
#include <interprocess/shared_object_name.hpp>

#include <cstddef>

#pragma once

namespace interprocess {

class shared_blob_impl_t;

class shared_blob_t {
 public:
  /** Create an empty blob with no data inside. */
  inline shared_blob_t() noexcept = default;
  /** Allocate a new region of shared memory. */
  inline explicit shared_blob_t(std::size_t size) noexcept
      : shared_blob_t{size, shared_object_name_t{}} {}
  /** Allocate a new region of shared memory with the given name */
  explicit shared_blob_t(std::size_t size, const shared_object_name_t& name) noexcept;
  /** Lookup a region of shared memory by its name. */
  explicit shared_blob_t(const shared_object_name_t& name) noexcept;
  /** Copy constructor */
  shared_blob_t(const shared_blob_t& other) noexcept;
  /** Move constructor */
  inline shared_blob_t(shared_blob_t&& other) noexcept
      : impl_(std::exchange(other.impl_, nullptr)) {}
  /** Copy assignment */
  inline auto operator=(const shared_blob_t& other) noexcept -> shared_blob_t& {
    return *this = shared_blob_t(other);
  }
  /** Move assignment */
  inline auto operator=(shared_blob_t&& other) noexcept -> shared_blob_t& {
    std::swap(impl_, other.impl_);
    return *this;
  }
  /** Destroy a region of shared memory. */
  ~shared_blob_t() noexcept;
  /**
   * After a call to this function, it's illegal to pass / lookup this blob across processes.
   *
   * Depending on the implementation, this function may proof the program against leaking system
   * resources, such as file handles, in case of hard crashes.
   */
  void seal() noexcept;
  /** Get the global name of this blob, to pass it to other processes for sharing. */
  [[nodiscard]] auto name() const noexcept -> const shared_object_name_t*;
  /**  Pointer to the region of shared memory. */
  [[nodiscard]] auto data() noexcept -> void*;
  [[nodiscard]] auto data() const noexcept -> const void*;

 private:
  shared_blob_impl_t* impl_{nullptr};
};

}  // namespace interprocess
