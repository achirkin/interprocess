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
    new (r.head_ + kRoot) node_t{kTerminal, kTerminal, initial_size, 0};
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
    if (ptr != nullptr && ptr->size_p.load() > size) {
      auto node_idx = get_idx(ptr) + size;
      auto& leftover = create(node_idx, ptr->size_p.load() - size);
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
    std::atomic<index_t> next_idx{kTerminal};
    /**
     * A backup link to a next node, for the case when `next_idx` used to indicate that the node is
     * unreachable from the root.
     */
    std::atomic<index_t> backup_idx{kTerminal};
    /** Number of elements in the node to the right from its idx. */
    std::atomic<index_t> size_p{0};
    /** Number of elements in the node to the left from its idx. */
    std::atomic<index_t> size_n{0};
    /** Shift the node left and set the backup_idx to kTerminal. */
    [[nodiscard]] auto normalize() -> node_t* {
      auto sn = size_n.load();
      if (sn > 0) {
        return new (this - sn) node_t{kTerminal, kTerminal, size_p.load() + sn, 0};
      } else {
        backup_idx.store(kTerminal);
        return this;
      }
    }
    /** Get the actual size of the node. */
    [[nodiscard]] auto get_size() const -> std::size_t { return size_n.load() + size_p.load(); }
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
    return *(new (get(index)) node_t{kTerminal, kTerminal, size, 0});
  }

  /** Add the node to the local list of owned nodes (no thread safety). */
  inline void add_to_owned_list(node_t* node) {
    node = node->normalize();
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
    // Try to merge adjacent nodes, if any
    while (self + self->size_p.load() == node) {
      self->size_p.fetch_add(node->size_p.load());
      next_idx = node->next_idx.load();
      self->next_idx.store(next_idx);
      node = get(next_idx);
    }
  }

  /** Try to increment `self`'s visitor counter and return its new state. */
  inline auto enter(node_t* self) -> index_t {
    return self->next_idx.fetch_add(kVisited) + kVisited;
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
      if (node->size_p.load() >= size) {
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
        auto new_index = get(kRoot)->size_p.fetch_add(size);
        grow(new_index + size);
        auto* new_node = new (get(new_index)) node_t{kTerminal, kTerminal, size, 0};
        return new_node;
      }
      // Otherwise, continue to look
      // NB: the actor needs to check if this node is deleted and use `backup_idx` accordingly
      bool self_is_deleted = (self_val_observed & kPtrMask) <= get_idx(self);
      next = get(self_is_deleted ? self->backup_idx.load() : self_val_observed);
      // If `next` is big enough, just cut it in-place.
      // NB: cutting `next` only on the left; cutting it on the right would hurt performance;
      //     probably too many small nodes being created.
      auto next_size_n = next->size_n.load();
      while (next_size_n >= size) {
        if (next->size_n.compare_exchange_weak(next_size_n, next_size_n - size)) {
          leave(self, self_val_observed);
          return new (next - next_size_n) node_t{kTerminal, kTerminal, size, 0};
        }
      }
      auto next_val_observed = enter(next);
      // Try to take `next` if:
      //   1. It's big enough
      //   2. This actor is the only visitor of `self`.
      //   3. (optimization) `self` is not deleted - otherwise skip and try to leave it.
      bool fits_size = next->size_p.load() + next_size_n >= size;
      bool needs_merge = self + self->size_p.load() + next_size_n == next;
      if (!self_is_deleted && (fits_size || needs_merge) &&
          (self_val_observed & kTagMask) == kVisited) {
        auto self_val_expected = self_val_observed;
        auto self_val_desired = (next_val_observed & kPtrMask) | kVisited;
        if (self->next_idx.compare_exchange_strong(self_val_expected, self_val_desired)) {
          // By calling mark-deleted, the actor effectively transfers the ownership of `self`.
          // step on the next and continue as usual.
          mark_deleted(next, next_val_observed, get_idx(self));
          self = next;
          self_val_observed = next_val_observed;
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
    node_t* self = get(kRoot);
    auto node_idx = get_idx(&node);
    auto node_size = node.size_p.load();
    {
      // special case: check if the node is on the edge of the list
      auto expected_list_size = node_idx + node_size;
      if (self->size_p.compare_exchange_strong(expected_list_size, node_idx)) {
        return true;
      }
    }
    auto self_val_observed = enter(self);
    while (true) {
      assert(self < &node);                   // `node` is to be inserted after `self`
      assert(self_val_observed >= kVisited);  // The actor must have visited `self`
      auto next_idx = self_val_observed & kPtrMask;
      if (next_idx <= get_idx(self)) {
        // The node has been deleted while the actor tried to insert past it.
        // Need to try again.
        leave(self, next_idx);
        return false;
      }
      // The actor can make several attempts to insert the node
      while (next_idx > node_idx) {
        {
          // Special case: glue the `node` to self.
          index_t expected_self_size_p = &node - self;
          if (self->size_p.compare_exchange_strong(expected_self_size_p,
                                                   expected_self_size_p + node_size)) {
            // Successfully enlarged `self`; check if it can be merged with `next`.
            if ((self_val_observed & kTagMask) == kVisited && next_idx < kTerminal) {
              expected_self_size_p += node_size;
              auto next = get(next_idx);
              if (self + expected_self_size_p + next->size_n.load() == next) {
                auto next_val_observed = enter(next);
                auto self_val_expected = self_val_observed;
                auto self_val_desired = (next_val_observed & kPtrMask) | kVisited;
                if (self->next_idx.compare_exchange_strong(self_val_expected, self_val_desired)) {
                  // By calling mark-deleted, the actor effectively transfers the ownership of
                  // `self`. step on the next and continue as usual.
                  mark_deleted(next, next_val_observed, get_idx(self));
                  self = next;
                  self_val_observed = next_val_observed;
                  next_idx = self_val_observed & kPtrMask;
                } else {
                  leave(next, next_val_observed);
                }
              }
            }
            leave(self, next_idx);
            return true;
          }
        }
        if (next_idx < kTerminal) {
          // Special case: glue the `node` to next.
          index_t expected_next_size_n = next_idx - node_idx - node_size;
          if (get(next_idx)->size_n.compare_exchange_strong(expected_next_size_n,
                                                            expected_next_size_n + node_size)) {
            leave(self, next_idx);
            return true;
          }
        }
        // Copy the visitor from `self` except the current actor
        // (no need to enter the inserted node).
        node.next_idx.store(self_val_observed - kVisited);
        // Replace the link from `self` to `node` while also leaving it.
        // At the same time, copy the other visitors.
        auto self_val_desired = ((self_val_observed & kTagMask) - kVisited) | node_idx;
        if (self->next_idx.compare_exchange_strong(self_val_observed, self_val_desired)) {
          // Succesfully replaced the value
          return true;
        }
        // What can go wrong?
        //   a. Changed number of visitors - need to copy to `node` again
        //   b. Inserted a new node after `self` - need to check the order again and leave the
        //      following nodes.
        //   c. `self` has been deleted
        if (next_idx != (self_val_observed & kPtrMask)) {
          if ((self_val_observed & kPtrMask) <= get_idx(self)) {
            // `self` has been deleted!
            leave(self, next_idx);
            return false;
          }
          // Definitely something is inserted.
          leave(get(self_val_observed), next_idx);
          next_idx = self_val_observed & kPtrMask;
        }
      }
      // By this point in code, `node` definitely must go after `next`
      auto next = get(next_idx);
      auto next_val_observed = enter(next);
      // ------------------ experiment -----------------------------------------------
      // if ((self_val_observed & kTagMask) == kVisited &&
      //     self + self->size_p.load() + next->size_n.load() == next) {
      //   auto self_val_desired = (next_val_observed & kPtrMask) | kVisited;
      //   if (self->next_idx.compare_exchange_strong(self_val_observed, self_val_desired)) {
      //     // By calling mark-deleted, the actor effectively transfers the ownership of
      //     // `self`. step on the next and continue as usual.
      //     mark_deleted(next, next_val_observed, get_idx(self));
      //     self = next;
      //     self_val_observed = next_val_observed;
      //     continue;
      //   }
      // }
      // // ------------------ experiment -----------------------------------------------
      leave(self, next_idx);
      self = next;
      self_val_observed = next_val_observed;
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
