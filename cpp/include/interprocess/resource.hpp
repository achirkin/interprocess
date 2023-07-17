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
    new (r.head_ + kRoot) node_t{kTerminal, initial_size};
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
  [[nodiscard]] auto from_memory_offset(std::ptrdiff_t diff) const -> T* {
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
    std::atomic<index_t> next_idx;
    std::atomic<index_t> size;

    /**
     * Replace the pointer part of the `next_idx` and capture its latest previous state.
     *
     * @return the new state.
     */
    inline auto replace_pointer(index_t& val_observed, index_t val_new) -> index_t {
      do {
        val_new = (val_observed & kTagMask) | (val_new & kPtrMask);
      } while (!next_idx.compare_exchange_weak(val_observed, val_new));
      return val_new;
    }
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
    return *(new (get(index)) node_t{kTerminal, size});
  }

  /** Merge owned lists (no tags / safety). */
  inline auto unsafe_merge(index_t left_idx, index_t right_idx) -> index_t {
    //  Prerequisite: both lists end with kTerminal.
    node_t fake_root{kTerminal, 0};
    node_t* tail = &fake_root;
    while (left_idx != kTerminal && right_idx != kTerminal) {
      node_t* left = get(left_idx);
      node_t* right = get(right_idx);
      if (left < right) {
        tail->next_idx.store(left_idx);
        tail = left;
        left_idx = left->next_idx.load();
      } else {
        tail->next_idx.store(right_idx);
        tail = right;
        right_idx = right->next_idx.load();
      }
    }
    // By this point either of the two lists is surely empty.
    if (left_idx != kTerminal) {
      tail->next_idx.store(left_idx);
    } else {
      tail->next_idx.store(right_idx);
    }
    return fake_root.next_idx.load();
  }

  /** Try to increment `self`'s visitor counter and return its new state. */
  inline auto enter(node_t* self) -> index_t {
    return self->next_idx.fetch_add(kVisited) + kVisited;
  }

  /**
   * Decrement `self`'s visitor counter, possibly take ownership of the node (saved in
   * `owned_nodes_`). The node must be visited via its `next_idx` counter.
   */
  inline auto leave_one(node_t* self) -> index_t {
    /*
    Possible state:
      a. [most likely] Just decrement the counter; the pointer value does not change.
         There may or may not be other visitors, none of the actor's concern.
         No extra action required.
      b. Another node(s) was inserted after `self`; this is detected by the changed pointer.
         The pointer is still ordered (ptr > get_idx(self)).
         CONSEQUENCE: the actor is automatically visiting the inserted node.
      c. `self` was deleted, but there are other actors in the node; this is detected by
         the pointer being equal to `kRoot`.
         No extra action required.
      d. `self` was deleted, and the actor is the last one to visit it; detected same as (c),
         plus no visitors
         CONSEQUENCE: the actor adds the node to `owned_nodes_`.
      e. Upon a dummy (nullptr) node, the actor does nothing and returns kTerminal.

    Not possible states:
      - The node given by `self_val_observed` cannot be deleted, even if multiple nodes were
        inserted in between, because no node can be deleted if there are any visitors to its
        predecessor.
    */
    if (self == nullptr) {
      return kTerminal;  // (e) Shortcut for dummy nodes
    }
    auto self_val_observed = self->next_idx.fetch_sub(kVisited);
    assert((self_val_observed & kTagMask) > 0);
    self_val_observed -= kVisited;
    auto ptr_observed = self_val_observed & kPtrMask;
    auto tag_observed = self_val_observed & kTagMask;
    if (ptr_observed == kRoot) {
      if (tag_observed == 0) {
        // (d). The node was deleted and this actor is the last visitor. Own it.
        self->next_idx.store(kTerminal);
        owned_nodes_ = unsafe_merge(owned_nodes_, get_idx(self));
      }
      return kTerminal | tag_observed;
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
    limit &= kPtrMask;
    index_t idx = kRoot;
    do {
      grow(idx + 1);
      idx = leave_one(self) & kPtrMask;
      self = head_ + idx;
    } while (idx < limit);
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
      // Special case: the last node
      //  - It's never deleted
      //  - It points past the last allocated element
      //  - Actors do not need to enter/leave it (the tag is ignored completely)
      if ((self_val_observed & kPtrMask) == kTerminal) {
        auto new_index = get(kRoot)->size.fetch_add(size);
        grow(new_index + size);
        auto* new_node = new (get(new_index)) node_t{kTerminal, size};
        leave(self, self_val_observed);
        return new_node;
      }
      // otherwise, continue to look
      next = get(self_val_observed);
      auto next_val_observed = enter(next);
      // Try to take `next` if:
      //   1. It's big enough
      //   2. This actor is the only visitor.
      if (next->size.load() >= size && (self_val_observed & kTagMask) == kVisited) {
        auto self_val_expected = self_val_observed;
        auto self_val_desired = (next_val_observed & kPtrMask) | kVisited;
        if (self->next_idx.compare_exchange_strong(self_val_expected, self_val_desired)) {
          self_val_observed = self_val_desired;
          // Succesfully removed `next`.
          // If the actor now leaves `next` last, they will own it.
          // To notify other actors that `next` is deleted, replace its pointer with the root:
          // the changed, unordered, link means the node has been severed from the list.
          next->replace_pointer(next_val_observed, kRoot);
          // Pass the previous state of `next` (just before short-circuited to the root), so that
          // the actor can leave and own it as usual.
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
  inline void insert(node_t& node) {
    node_t* self = get(kRoot);
    node_t* prev = nullptr;
    auto node_idx = get_idx(&node);
    auto self_val_observed = enter(self);
    auto prev_val_observed = kRoot;
    while (true) {
      assert(self < &node);
      auto next_idx = self_val_observed & kPtrMask;
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
        if (!self->next_idx.compare_exchange_weak(self_val_observed, self_val_desired)) {
          // Succesfully replaced the value
          leave(prev, prev_val_observed);
          return;
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
