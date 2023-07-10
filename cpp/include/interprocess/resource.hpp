#include <interprocess/blob.hpp>
#include <interprocess/common.hpp>

#include <atomic>
#include <cassert>
#include <cstddef>
#include <limits>
#include <new>
#include <optional>

#pragma once

namespace interprocess {

template <typename T>
struct resource_t {
 public:
  /** Create a new shared memory resource. */
  static inline auto create(std::optional<std::size_t> max_size = std::nullopt,
                            const shared_object_name_t& name = {}) noexcept -> resource_t {
    auto initial_size = std::max(kRoot, kTerminal) + 1;
    auto r = resource_t{blob_t::create(sizeof(T) * initial_size, max_size, name)};
    new (r.head_ + kRoot) node_t{kTerminal, 0};
    new (r.head_ + kTerminal) node_t{initial_size, kPtrMask};
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
    if (ptr != nullptr && ptr->size > size) {
      auto node_idx = get_idx(*ptr) + size;
      auto& leftover = create(node_idx, ptr->size - size);
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
      auto node_idx = get_idx(*ptr) + new_size;
      auto& leftover = create(node_idx, old_size - new_size);
      insert(leftover);
      cleanup();
      return ptr;
    }
    auto* new_ptr = pop_first(new_size);
    if (new_ptr != nullptr && ptr->size > new_size) {
      auto node_idx = get_idx(*ptr) + new_size;
      auto& leftover = create(node_idx, ptr->size - new_size);
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

 private:
  using index_t = std::size_t;

  constexpr static inline index_t kTagBits = 4 + sizeof(void*);
  constexpr static inline index_t kTagShift = std::numeric_limits<index_t>::digits - kTagBits;
  constexpr static inline index_t kTagMask = ((index_t{1} << kTagBits) - 1) << kTagShift;
  /** Maximum size of the list. */
  constexpr static inline index_t kPtrMask = ~kTagMask;
  /** Pointer tag bits saying that the node is being removed. */
  constexpr static inline index_t kDeleted = index_t{1} << kTagShift;
  /** The rest of the bits are used for visit counter. */
  constexpr static inline index_t kVisited = kDeleted << 1;
  /** All bits that belong to visitor counter. */
  constexpr static inline index_t kVisitMask = kTagMask & (~kDeleted);
  /** Index of the special root node. */
  constexpr static inline index_t kRoot = 1;
  /** Index of the special last node. It points to the first element past the allocated area. */
  constexpr static inline index_t kTerminal = 0;

  /*
    All currently available memory is stored in the linked list as defined by node_t.
    All nodes are ordered the size, ties are resolved by the index.
    [Ordering invariants]
      - Root node is always first (zero size).
      - There always exists the last node;
        for convenience it's linked to the root; only one element of it is actually allocated.

    The next_idx is the core of the linked list. Besides the index of the next node, it
    packs a tag that contains the deletion mark and the visitor counter. The deletion mark tells
    whether the connection to this from the previous node should be severed. The visitor counter
    protects an unreachable (w.r.t. kRoot) node from being owned by an actor (and thus trashed).
    [Invariants]
      1. A visited node or it's next both cannot be trashed.
      2. The `next_idx` pointer part cannot be changed on a node marked for deletion.

    [Notes]
    Inv.2 to restricts the insertion and severing:
      - The severing of a chain of deleted nodes must be done in the order of their appearance
        (TBD: all at once? hard to track visits then)
      - The insertion must happen in front of the node, not behind. This also implies the order
        is broken in a deleted section of the list.
   */
  struct alignas(sizeof(T)) node_t {
    std::atomic<index_t> next_idx;
    index_t size;

    // There is a strict order of node_t given by a lexicographic comparison of (size, addressof).
    // This also implies a node_t can only ever be equal to itself.
    friend constexpr inline auto operator==(const node_t& l, const node_t& r) noexcept -> bool {
      return &l == &r;
    }
    friend constexpr inline auto operator!=(const node_t& l, const node_t& r) noexcept -> bool {
      return &l != &r;
    }
    friend constexpr inline auto operator<=(const node_t& l, const node_t& r) noexcept -> bool {
      return (l.size == r.size) ? &l <= &r : l.size < r.size;
    }
    friend constexpr inline auto operator>=(const node_t& l, const node_t& r) noexcept -> bool {
      return (l.size == r.size) ? &l >= &r : l.size > r.size;
    }
    friend constexpr inline auto operator<(const node_t& l, const node_t& r) noexcept -> bool {
      return (l.size == r.size) ? &l < &r : l.size < r.size;
    }
    friend constexpr inline auto operator>(const node_t& l, const node_t& r) noexcept -> bool {
      return (l.size == r.size) ? &l > &r : l.size > r.size;
    }

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

    /** Set the deletion mark and get the new observed state. */
    inline void mark_delete(index_t& val_observed) {
      while (!next_idx.compare_exchange_weak(val_observed, val_observed | kDeleted)) {
      }
      val_observed |= kDeleted;
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

  [[nodiscard]] inline auto is_deleted(index_t i) const -> bool { return (i & kDeleted) != 0; }

  [[nodiscard]] inline auto get_idx(const node_t& node) const -> index_t {
    return index_t(&node - head_);
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
      if (*left < *right) {
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

  /** Try to increment `self`'s visitor counter and return its state. */
  inline auto enter(node_t& self) -> index_t {
    return self.next_idx.fetch_add(kVisited) + kVisited;
  }

  /**
   * Decrement `self`'s visitor counter, possibly take ownership of one or more nodes.
   * The node must be visited via its `next_idx` counter.
   *
   * @param just_severed try to own the node even if the `self` value hasn't changed
   */
  inline void leave(node_t& self, index_t& self_val_observed, bool just_severed = false) {
    assert((self_val_observed & kVisitMask) > 0);
    auto self_ref_orig = self_val_observed & kPtrMask;
    self_val_observed = self.next_idx.fetch_sub(kVisited) - kVisited;

    bool ref_changed = (self_val_observed & kPtrMask) != self_ref_orig;
    bool last_in_deleted = (self_val_observed & kTagMask) == kDeleted;
    if (last_in_deleted && (ref_changed || just_severed)) {
      // 1. self is deleted
      // 2. this actor is the last visitor
      // 3. reference to next is changed
      //    => the actor owns the node
      self.next_idx.store(kTerminal);
      owned_nodes_ = unsafe_merge(owned_nodes_, get_idx(self));
    }

    if (ref_changed) {
      // Whether a node was inserted or severed, the visitor counter has been copied;
      // we need to leave that node as well.
      // Potentially, this could repeat multiple times.
      auto& next = *get(self_val_observed);
      auto next_val_observed = next.next_idx.load();
      return leave(next, next_val_observed);
    }
  }

  /**
   * Try to sever the connection between `self` and `next`.
   * Both nodes must be entered before the call to this function.
   *
   * @return whether severing was successful.
   */
  inline auto try_sever(node_t& self, index_t& self_val_observed, node_t& next,
                        index_t& next_val_observed) -> bool {
    /*
    [Prerequisites]
      1. The `next` is marked deleted.
      2. The `self` is NOT marked deleted.
      3. This actor is the only visitor registered by `self`.
    */
    if (!is_deleted(next_val_observed)) {
      return false;
    }
    if (self_val_observed != (get_idx(next) | kVisited)) {
      return false;
    }
    // Assuming the actor is still the only visitor to the severed node (next) and it's still marked
    // for deletion, cut the link.
    if (!self.next_idx.compare_exchange_strong(self_val_observed, next_val_observed & ~kDeleted)) {
      // If anything to the node has changed, it's no longer eligible for severing.
      return false;
    }
    // Save the latest snapshot of the observed link.
    self_val_observed = next_val_observed & ~kDeleted;

    // Now that the `next` is severed, the actor needs to make sure the visit counters are in sync.
    // Replace the pointer part of the `next`'s link, while observing changes to its tag (if any).
    next_val_observed = next.replace_pointer(next_val_observed, get_idx(self));

    // The only part of the value of the node marked as deleted (`next`) that can change is the
    // visitor counter.
    auto visit_delta = (self_val_observed & kVisitMask) - (next_val_observed & kVisitMask);
    self.next_idx.fetch_sub(visit_delta);

    return true;
  }

  inline auto pop_first_local(std::size_t size) -> node_t* {
    auto prev_idx = kRoot;
    auto self_idx = owned_nodes_ & kPtrMask;
    while (self_idx != kTerminal) {
      auto* node = get(self_idx);
      if (node->size >= size) {
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
    auto self_val_observed = enter(*self);
    node_t* next = nullptr;
    while (true) {
      // Check if the actor has owned a large enough node
      auto* local = pop_first_local(size);
      if (local != nullptr) {
        leave(*self, self_val_observed);
        return local;
      }
      next = get(self_val_observed);
      // Special case: the last node
      //  - It's never marked/deleted
      //  - It points past the last allocated element
      //  - Actors do not need to enter/leave it (the tag is ignored completely)
      if ((self_val_observed & kPtrMask) == kTerminal) {
        auto new_index = next->next_idx.fetch_add(size);
        grow(new_index + size);
        auto* new_node = new (get(new_index)) node_t{kTerminal, size};
        leave(*self, self_val_observed);
        return new_node;
      }
      // otherwise, continue to look
      auto next_val_observed = enter(*next);
      // If `self` is not marked deleted, the actor can try to pick `next`;
      // otherwise, don't bother and skip.
      if (!is_deleted(self_val_observed)) {
        // Mark next for deletion if `next` is large enough
        if (size <= next->size) {
          next->mark_delete(next_val_observed);
        }

        // Try to cut off the `next` if it's marked deleted
        if (try_sever(*self, self_val_observed, *next, next_val_observed)) {
          leave(*next, next_val_observed, true);
          continue;
        }
      }
      // Go to next node.
      leave(*self, self_val_observed);
      self = next;
      self_val_observed = next_val_observed;
    }
  }

  /** Insert a node. */
  inline void insert(node_t& node) {
    while (!try_insert(node)) {
    }
  }

  /**
   * A single attempt to insert a node in a list.
   * This may fail due to concurrent changes in the list.
   * For example, the actor needs to start over when the node at the insertion point is marked for
   * deletion.
   *
   * @return whether the insertion succeeded
   */
  inline auto try_insert(node_t& node) -> bool {
    node_t* self = get(kRoot);
    auto self_val_observed = enter(*self);
    node_t* next = nullptr;
    // In this loop the actor advances over the list trying to insert the node between self and
    // next, always checking that self is not deleted.
    while (true) {
      assert(!is_deleted(self_val_observed) && *self < node);
      next = get(self_val_observed);
      auto next_val_observed = enter(*next);
      // Try to cut off the `next` if it's marked deleted
      if (try_sever(*self, self_val_observed, *next, next_val_observed)) {
        leave(*next, next_val_observed, true);
        continue;
      }
      // Actor looks for the first non-deleted node to compare against.
      // A chain of marked (deleted) nodes is not ordered, but it's true that all
      // nodes in the chain are smaller than their first non-deleted following neighbor.
      // Hence, if any of the marked nodes is greater than the `node`, the next non-deleted
      // neighbor is also greater than the `node`, hence the `node` can be inserted after `self`.
      while (is_deleted(next_val_observed) && *next < node) {
        auto& new_next = *get(next_val_observed);
        auto new_next_val_observed = enter(new_next);
        leave(*next, next_val_observed);
        next = &new_next;
        next_val_observed = new_next_val_observed;
      }
      // Deleted or not, if `node < *next`, the actor can try to insert the node after `self`.
      if (get_idx(*next) == kTerminal || node < *next) {
        leave(*next, next_val_observed);
        if (try_insert_at(*self, self_val_observed, node)) {
          // Success! Now leave.
          leave(*self, self_val_observed);
          return true;
        } else if (is_deleted(self_val_observed)) {
          // The `self` has been deleted in the meanwhile! Have to retry from the root
          leave(*self, self_val_observed);
          return false;
        } else {
          // Something could have been inserted after `self`; repeat the outer loop.
          continue;
        }
      }
      // If this point is reached, the `next` must not be marked deleted, and it still precedes the
      // `node`; the actor can advance independent of the state of current `self`.
      leave(*self, self_val_observed);
      self = next;
      self_val_observed = next_val_observed;
    }
  }

  /**
   * Try to insert a node right after `self`.
   * It's assumed and not checked that `self` is the right place:
   *   - self < node
   *   - node < next', where next' is the first non-deleted node after `self`.
   *
   * It may fail in two ways:
   *   - if `is_deleted`, need to start over from the root node.
   *   - otherwise, can attempt somewhere starting from `self`.
   *
   * @return whether insertion was successful
   */
  inline auto try_insert_at(node_t& self, index_t& self_val_observed, node_t& node) -> bool {
    assert((self_val_observed & kVisitMask) > 0);
    index_t next_idx_orig = self_val_observed & kPtrMask;
    while (true) {
      if (is_deleted(self_val_observed)) {
        // Failed and have to start over from the root, because the actor cannot go back the list
        // indefinitely
        return false;
      }
      if ((self_val_observed & kPtrMask) != next_idx_orig) {
        // A new node has been inserted in the meanwhile
        auto& next = *get(self_val_observed);
        if (node < next) {
          // The actor is lucky enough that `self` is still the right place for the `node`
          next_idx_orig = self_val_observed & kPtrMask;
        } else {
          // It's unclear where to insert the node, because any number of nodes could have been
          // inserted between `self` and `node` by now.
          return false;
        }
      }
      // Try to insert the node
      // Copy both the visitors except the current actor - it does not enter the inserted node,
      // as he knows the newest value of `self` already.
      node.next_idx.store(self_val_observed - kVisited);
      auto self_val_desired = (self_val_observed & kVisitMask) | get_idx(node);
      if (self.next_idx.compare_exchange_weak(self_val_observed, self_val_desired)) {
        // Successfully inserted the node!
        self_val_observed = self_val_desired;
        return true;
      }
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
};

}  // namespace interprocess