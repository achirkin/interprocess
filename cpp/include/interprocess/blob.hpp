
#include <interprocess/shared_object_name.hpp>

#include <cstddef>
#include <memory>
#include <optional>
#include <utility>

#pragma once

namespace interprocess {

class blob_impl_t;

/**
 * Shared memory blob reserves the requested address range in each process,
 * but starts allocating only a small chunk of shared memory.
 *
 * Thus it can be used as a memory pool, claiming the memory chunk-by-chunk
 * and releasing it all at once at destruction of the object.
 *
 * All of the blob methods must be IPC-safe, so it can be used as a building
 * block for other IPC abstractions.
 */
class blob_t {
 public:
  /**
   * Create a new shared memory blob.
   *
   * @param initial_size
   *    optinal initial blob size in bytes;
   *    if not given, a reasonable initial size is determined automatically.
   *
   * @param max_size
   *    optional maximum blob size in bytes;
   *    if not given, a reasonable maximum size is determined automatically.
   *    The blob reserves an address range equal to the given maximum size once at creation.
   *
   * @param name
   *    optional name;
   *    if not given, the name is generated randomly.
   */
  static auto create(std::optional<std::size_t> initial_size = std::nullopt,
                     std::optional<std::size_t> max_size = std::nullopt,
                     const shared_object_name_t& name = {}) noexcept -> blob_t;

  /** Look up an existing shared memory resource. */
  static auto lookup(const shared_object_name_t& name) noexcept -> blob_t;

  inline blob_t() noexcept = delete;
  /** Copy constructor */
  blob_t(const blob_t& other) noexcept;
  /** Move constructor */
  inline blob_t(blob_t&& other) noexcept : impl_(std::exchange(other.impl_, nullptr)) {}
  /** Copy assignment */
  inline auto operator=(const blob_t& other) noexcept -> blob_t& { return *this = blob_t(other); }
  /** Move assignment */
  inline auto operator=(blob_t&& other) noexcept -> blob_t& {
    std::swap(impl_, other.impl_);
    return *this;
  }
  /** Destroy a region of shared memory. */
  ~blob_t() noexcept;
  /**
   * After a call to this function, it's illegal to pass / lookup this blob across processes.
   *
   * Depending on the implementation, this function may proof the program against leaking system
   * resources, such as file handles, in case of hard crashes.
   */
  void seal() noexcept;
  /**
   * Attempt to increase the size of the shared object and return the new size in bytes.
   * The returned new size may be different from the requested size for various reasons:
   *
   *  - If the requested size is smaller than the actual size
   *  - If the requested size is larger than the `max_size`
   *  - If the OS allocation fails for any reason
   */
  auto grow(std::size_t size) noexcept -> std::size_t;
  /** Get the global name of this blob, to pass it to other processes for sharing. */
  [[nodiscard]] auto name() const noexcept -> const shared_object_name_t*;
  /** Pointer to the region of shared memory. */
  [[nodiscard]] auto data() noexcept -> void*;
  [[nodiscard]] auto data() const noexcept -> const void*;

 private:
  blob_impl_t* impl_{nullptr};
  inline blob_t(blob_impl_t* impl) : impl_{impl} {}
};

}  // namespace interprocess
