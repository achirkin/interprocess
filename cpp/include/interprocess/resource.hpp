#pragma once

#include <interprocess/blob.hpp>
#include <interprocess/common.hpp>

#include <atomic>
#include <cassert>
#include <cstddef>
#include <limits>
#include <new>
#include <optional>

namespace interprocess {

template <typename T>
struct resource_t {
 public:
  inline resource_t() noexcept = delete;
  inline resource_t(const resource_t& other) noexcept : blob_{other.blob_} {};
  inline auto operator=(const resource_t& other) noexcept -> resource_t& {
    return *this = resource_t{other};
  }
  inline resource_t(resource_t&& other) noexcept = default;
  inline auto operator=(resource_t&& other) noexcept -> resource_t& = default;
  inline ~resource_t() noexcept = default;

  /** Create a new shared memory resource. */
  static inline auto create(std::optional<std::size_t> max_size = std::nullopt,
                            const shared_object_name_t& name = {}) noexcept -> resource_t {
    auto initial_size = kRoot + 1;
    auto r = resource_t{blob_t::create(sizeof(node_t) * initial_size, max_size, name)};
    new (r.head_ + kRoot) node_t{kTerminal, initial_size, kTerminal};
    return r;
  }

  /** Look up an existing shared memory resource. */
  static auto lookup(const shared_object_name_t& name) noexcept -> resource_t {
    return resource_t{blob_t::lookup(name)};
  }

  auto allocate(std::size_t size) -> T* {
    INTERPROCESS_LOG_DEBUG("allocate %zu\n", size);
    if (size == 0) {
      return nullptr;
    }
    auto* ptr = pop_first(size);
    if (ptr != nullptr && ptr->size.load() > size) {
      auto node_idx = get_idx(ptr) + size;
      auto& leftover = create(node_idx, ptr->size.load() - size);
      insert(leftover);
    }
    cleanup();
    return reinterpret_cast<T*>(ptr);
  }

  auto reallocate(const T* ptr, std::size_t old_size, std::size_t new_size) -> T* {
    INTERPROCESS_LOG_DEBUG("reallocate %p: %zu -> %zu\n", reinterpret_cast<const void*>(ptr),
                           old_size, new_size);
    if (ptr == nullptr || old_size == 0) {
      return allocate(new_size);
    }
    if (new_size == 0) {
      deallocate(ptr, old_size);
      return nullptr;
    }
    if (old_size == new_size) {
      return ptr;
    }
    if (old_size > new_size) {
      auto node_idx = get_idx(ptr) + new_size;
      auto& leftover = create(node_idx, old_size - new_size);
      insert(leftover);
      cleanup();
      return ptr;
    }
    auto* new_ptr = pop_first(new_size);
    if (new_ptr != nullptr && ptr->size.load() > new_size) {
      auto node_idx = get_idx(ptr) + new_size;
      auto& leftover = create(node_idx, ptr->size.load() - new_size);
      insert(leftover);
      memcpy(new_ptr, ptr, sizeof(T) * new_size);
      node_idx = index_t(reinterpret_cast<const node_t*>(ptr) - head_);
      auto& new_node = create(node_idx, old_size);
      insert(new_node);
    }
    cleanup();
    return new_ptr;
  }

  void deallocate(const T* ptr, std::size_t size) {
    if (ptr == nullptr || size == 0) {
      return;
    }
    auto node_idx = index_t(reinterpret_cast<const node_t*>(ptr) - head_);
    auto& new_node = create(node_idx, size);
    insert(new_node);
    cleanup();
    INTERPROCESS_LOG_DEBUG("deallocate %p: %zu; owned_nodes: %zu\n",
                           reinterpret_cast<const void*>(ptr), size, owned_nodes_);
  }

  /** Return an offset of this pointer w.r.t, to the shared memory area. */
  [[nodiscard]] auto get_memory_offset(const T* ptr) const -> std::ptrdiff_t {
    return ptr - reinterpret_cast<T*>(head_);
  }

  /** Return an offset of this pointer w.r.t, to the shared memory area. */
  [[nodiscard]] auto from_memory_offset(std::ptrdiff_t diff) -> T* {
    grow(diff + 1);
    return reinterpret_cast<T*>(head_) + diff;
  }

 private:
  using index_t = std::size_t;

  constexpr static inline index_t kTagBits = 4 + sizeof(void*);
  constexpr static inline index_t kPtrBits = std::numeric_limits<index_t>::digits - kTagBits;
  constexpr static inline index_t kTagMask = ((index_t{1} << kTagBits) - 1) << kPtrBits;
  /** Maximum size of the list. */
  constexpr static inline index_t kPtrMask = ~kTagMask;
  /** The rest of the bits are used for visit counter. */
  constexpr static inline index_t kVisited = index_t{1} << kPtrBits;
  /** Index of the special root node. */
  constexpr static inline index_t kRoot = 0;
  /** Index of the special last node. It does not exist and must never be visited. */
  constexpr static inline index_t kTerminal = kPtrMask;

  /*
    All currently available memory is stored in the linked list as defined by node_t.
    All nodes are ordered by their index/pointer.

    The next_idx is the core of the linked list. Besides the index of the next node, it
    packs a tag that contains the visitor counter. The visitor counter
    protects an unreachable (w.r.t. kRoot) node from being owned by an actor (and thus trashed).
    [Invariant]
      - A connection between a visited node and its next cannot be severed.
   */
  struct alignas(sizeof(T)) node_t {
    /**
     * Pack the visitor counter and an index.
     * In the normal state, it points to the next node, hence `next_idx > self_idx` (assuming no
     * tag). When the node is unreachable from the root, but the next node is reachable from the
     * root, `next_idx < self_idx` (in fact, it points to the previous - last reachable node). When
     * both self and next nodes are unreachable from the root, `next_idx == self_idx`.
     */
    std::atomic<index_t> next_idx;
    /** Number of elements in the node */
    std::atomic<index_t> size;
    /**
     * A backup link to a next node, for the case when `next_idx` used to indicate that the node is
     * unreachable from the root.
     */
    std::atomic<index_t> backup_idx;
  };

  blob_t blob_;
  node_t* head_{reinterpret_cast<node_t*>(blob_.data())};
  index_t owned_nodes_{kTerminal};
  index_t size_{0};

  explicit resource_t(blob_t&& blob) noexcept : blob_{std::move(blob)} {}

  static_assert(sizeof(T) == sizeof(node_t));
  static_assert(std::atomic<index_t>::is_always_lock_free,
                "This atomic should be lock-free for IPC.");

  inline void grow(index_t n) {
    if (size_ < n) /* unlikely */ {
      size_ = blob_.grow(n * sizeof(T)) / sizeof(T);
    }
  }

  [[nodiscard]] inline auto get(index_t i) -> node_t* {
    auto ix = i & kPtrMask;
    grow(ix + 1);
    return head_ + ix;
  }

  [[nodiscard]] inline auto get_idx(const node_t* node) const -> index_t {
    return index_t(node - head_);
  }

  /** Inits the node in the memory; sets everything except next_idx. */
  inline auto create(index_t index, index_t size) -> node_t& {
    return *(new (get(index)) node_t{kTerminal, size, kTerminal});
  }

  /** Add the node to the local list of owned nodes (no thread safety). */
  inline void add_to_owned_list(node_t* node) {
    node->backup_idx.store(kTerminal);
    auto node_idx = get_idx(node);
    if (node_idx < owned_nodes_) {
      node->next_idx.store(owned_nodes_);
      owned_nodes_ = node_idx;
      return;
    }
    auto self = get(owned_nodes_);
    index_t next_idx = self->next_idx.load();
    while (next_idx < node_idx) {
      self = get(next_idx);
      next_idx = self->next_idx.load();
    }
    node->next_idx.store(next_idx);
    self->next_idx.store(node_idx);
  }

  /** Try to increment `self`'s visitor counter and return its new state. */
  inline auto enter(node_t* self) -> index_t {
    auto self_val_observed = self->next_idx.fetch_add(kVisited) + kVisited;
    if ((self_val_observed & kPtrMask) <= get_idx(self)) {
      return (self_val_observed & kTagMask) | self->backup_idx.load();
    }
    return self_val_observed;
  }

  /**
   * Leave one or more nodes up to the one that points at or beyond the limit.
   * All of the nodes must be visited via their `next_idx` counters.
   *
   * @param self - the first node to leave
   * @param limit - the first node _not_ to leave
   *                (does not need to exist, the tag is ignored)
   */
  inline void leave(node_t* self, index_t limit) {
    if (self == nullptr) {
      return;
    }
    limit &= kPtrMask;
    index_t self_idx = get_idx(self);
    index_t visited_count = kVisited;
    do {
      grow(self_idx + 1);
      index_t backup_idx = kTerminal;
      index_t self_val_observed, next_idx;
      // Leave the node and check its backup idx if necessary
      do {
        self_val_observed = self->next_idx.load();
        assert((self_val_observed & kTagMask) > 0);
        next_idx = self_val_observed & kPtrMask;
        if (next_idx <= self_idx) {
          backup_idx = self->backup_idx.load();
        }
      } while (!self->next_idx.compare_exchange_weak(self_val_observed,
                                                     self_val_observed - visited_count));
      self_val_observed -= visited_count;
      // At this point, the actor left `self`.
      // Either the actor owns `self` or cannot touch it anymore.
      if ((self_val_observed & kTagMask) > 0) {
        // not owning `self`. Just find a proper next `self_idx`
        if (next_idx <= self_idx) {
          self_idx = backup_idx;
        } else {
          self_idx = next_idx;
        }
        visited_count = kVisited;
      } else {
        if (next_idx <= self_idx) {
          // Owning the deleted `self`
          add_to_owned_list(self);
          if (next_idx == self_idx) {
            // The actor is the last visitor to a not-last node in the deleted chain.
            // Need to leave `next` extra time.
            if (limit <= backup_idx) {
              limit = backup_idx + 1;
              visited_count = kVisited;
            } else {
              visited_count = kVisited * 2;
            }
            self_idx = backup_idx;
          } else {
            // The actor is owning the last node  in a deleted chain.
            // Need to leave the `next_idx`, which is actually the predecessor to the deleted chain.
            limit = std::max(limit, backup_idx);
            self_idx = next_idx;
            visited_count = kVisited;
          }
        } else {
          visited_count = kVisited;
          self_idx = next_idx;
        }
      }
      self = head_ + self_idx;
    } while (self_idx < limit);
  }

  inline void mark_deleted(node_t* self, index_t limit, index_t prev_idx) {
    limit &= kPtrMask;
    prev_idx &= kPtrMask;
    index_t self_idx = get_idx(self);
    index_t extra_visit = 0;
    bool next_deleted = true;
    while (next_deleted) {
      index_t self_val_desired, next_idx;
      index_t self_val_observed = self->next_idx.load();
      do {
        next_idx = self_val_observed & kPtrMask;
        assert(next_idx > self_idx);
        next_deleted = next_idx < limit;
        self->backup_idx.store(next_idx);
        self_val_desired =
            ((self_val_observed + extra_visit) & kTagMask) | (next_deleted ? self_idx : prev_idx);
      } while (!self->next_idx.compare_exchange_weak(self_val_observed, self_val_desired));
      if (!next_deleted) return;
      extra_visit = kVisited;
      self_idx = next_idx;
      self = get(self_idx);
    }
  }

  inline auto pop_first_local(std::size_t size) -> node_t* {
    auto prev_idx = kRoot;
    auto self_idx = owned_nodes_ & kPtrMask;
    while (self_idx != kTerminal) {
      auto* node = get(self_idx);
      if (node->size.load() >= size) {
        if (prev_idx != kRoot) {
          get(prev_idx)->next_idx.store(node->next_idx.load());
        } else {
          owned_nodes_ = node->next_idx.load();
        }
        return node;
      }
      prev_idx = self_idx;
      self_idx = node->next_idx.load() & kPtrMask;
    }
    return nullptr;
  }

  /** Get the first node of at least the requested size. */
  inline auto pop_first(std::size_t size) -> node_t* {
    node_t* self = get(kRoot);
    auto self_val_observed = enter(self);
    node_t* next = nullptr;
    while (true) {
      // Check if the actor has owned a large enough node
      auto* local = pop_first_local(size);
      if (local != nullptr) {
        leave(self, self_val_observed);
        return local;
      }
      // Reached the end of the list: allocate new node.
      if ((self_val_observed & kPtrMask) == kTerminal) {
        leave(self, self_val_observed);
        auto new_index = get(kRoot)->size.fetch_add(size);
        grow(new_index + size);
        auto* new_node = new (get(new_index)) node_t{kTerminal, size, kTerminal};
        return new_node;
      }
      // otherwise, continue to look
      next = get(self_val_observed);
      auto next_val_observed = enter(next);
      assert((next_val_observed & kPtrMask) > (self_val_observed & kPtrMask));  // Not deleted
      // Try to take `next` if:
      //   1. It's big enough
      //   2. This actor is the only visitor of `self`.
      if (next->size.load() >= size && (self_val_observed & kTagMask) == kVisited) {
        auto self_val_expected = self_val_observed;
        auto self_val_desired = (next_val_observed & kPtrMask) | kVisited;
        if (self->next_idx.compare_exchange_strong(self_val_expected, self_val_desired)) {
          self_val_observed = enter(self);  // TODO: try not to enter second time
          mark_deleted(next, next_val_observed, get_idx(self));
          leave(next, next_val_observed);
          continue;
        }
      }
      // Go to next node.
      leave(self, self_val_observed);
      self = next;
      self_val_observed = next_val_observed;
    }
  }

  /** Insert a node. */
  inline auto try_insert(node_t& node) -> bool {
    // TODO: try to keep only one node visited most of the time.
    node_t* self = get(kRoot);
    node_t* prev = nullptr;
    auto node_idx = get_idx(&node);
    auto self_val_observed = enter(self);
    auto prev_val_observed = kRoot;
    while (true) {
      assert(self < &node);                   // `node` is to be inserted after `self`
      assert(self_val_observed >= kVisited);  // The actor must have visited `self`
      auto next_idx = self_val_observed & kPtrMask;
      if (next_idx <= get_idx(self)) {
        // The node has been deleted while the actor tried to insert past it.
        // Need to try again.
        leave(self, self_val_observed);
        leave(prev, prev_val_observed);
        return false;
      }
      // The actor can make several attempts to insert the node
      while (next_idx > node_idx) {
        // Copy the visitor from `self` except the current actor
        // (no need to enter the inserted node).
        node.next_idx.store(self_val_observed - kVisited);
        // Replace the link from `self` to `node` while also leaving it.
        // At the same time, copy the other visitors.
        // Note:
        //  - `self` cannot be removed, because it's protected by visiting `prev`
        auto self_val_desired = ((self_val_observed & kTagMask) - kVisited) | node_idx;
        if (self->next_idx.compare_exchange_weak(self_val_observed, self_val_desired)) {
          // Succesfully replaced the value
          leave(prev, prev_val_observed);
          return true;
        }
        // What can go wrong?
        //   a. Changed number of visitors - need to copy to `node` again
        //   b. Inserted a new node after `self` - need to check the order again and leave the
        //      following nodes.
        //   c. Deleting `self` or `next` is impossible due to visiting `prev` and`self`.
        if (next_idx != (self_val_observed & kPtrMask)) {
          // Definitely something is inserted.
          auto next_val_expected = (self_val_observed & kTagMask) | next_idx;
          leave(get(self_val_observed), next_val_expected);
          next_idx = self_val_observed & kPtrMask;
        }
      }
      // By this point in code, `node` definitely must go after `next`
      leave(prev, prev_val_observed);
      prev = self;
      prev_val_observed = self_val_observed;
      self = get(next_idx);
      self_val_observed = enter(self);
    }
  }

  inline void insert(node_t& node) {
    while (!try_insert(node))
      ;
  }

  /** Reinsert all locally owned nodes until the list is empty. */
  inline void cleanup() {
    while (owned_nodes_ != kTerminal) {
      auto& node = *get(owned_nodes_);
      owned_nodes_ = node.next_idx.load();
      insert(node);
    }
  }

#ifdef INTERPROCESS_STRUCT_INSPECT
  template <typename>
  friend class inspect_resource_t;
#endif
};

}  // namespace interprocess
