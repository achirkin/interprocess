#include <interprocess/array.hpp>
#include <interprocess/common.hpp>

#include <array>
#include <atomic>
#include <cstddef>
#include <cstdint>
#include <new>  // placement new

#pragma once

namespace interprocess {

template <typename From, typename To, typename = void>
struct is_reinterpretable : std::false_type {};

template <typename From, typename To>
struct is_reinterpretable<From, To,
                          std::void_t<decltype(reinterpret_cast<To>(std::declval<From>()))>>
    : std::true_type {};

template <typename From, typename To>
inline constexpr bool is_reinterpretable_v = is_reinterpretable<From, To>::value;  // NOLINT

/** Lock-free non-shrinking map */
template <std::uint8_t B, typename K, typename V>
class map_t {
 private:
  static constexpr auto rec_depth_bits(std::uint8_t acc) noexcept -> std::uint8_t {
    return std::size_t(kRadixBase << acc) >= kKeySize ? acc : rec_depth_bits(acc + 1);
  }
  static constexpr auto get_depth_bits() noexcept -> std::uint8_t { return rec_depth_bits(0); }

 public:
  using key_type = K;
  using value_type = V;
  using pointer_t = std::int8_t*;
  using symbol_type = std::uint16_t;
  using index_type = std::uint32_t;

  static constexpr std::uint8_t kRadixBase = B;
  static constexpr key_type kRadixMask = key_type{(1 << kRadixBase) - 1};
  static constexpr std::size_t kKeySize = sizeof(key_type) * 8;
  static constexpr std::size_t kSymSize = sizeof(symbol_type) * 8;

  static_assert(kRadixBase <= kSymSize, "Radix base must fit in the symbol size.");

  static constexpr std::uint8_t kDepthBits = get_depth_bits();
  static constexpr std::uint8_t kDepthMask = std::uint8_t{(1 << kDepthBits) - 1};
  static constexpr std::uint8_t kDepthMax = div_rounding_up<std::size_t>(kKeySize, kRadixBase);
  static constexpr std::uint8_t kTrailingBits = (kKeySize + kRadixBase - 1) % kRadixBase + 1;
  static constexpr std::uint8_t kPrefixBits = kKeySize - kDepthBits;

  static_assert(kDepthBits <= kTrailingBits,
                "Tree depth is too big because the radix base is too small.");

  map_t(const map_t& other) = delete;
  auto operator=(const map_t& other) = delete;
  map_t(map_t&& other) noexcept
      : nodes_{std::exchange(other.nodes_, growing_blob_t{})},
        leafs_{std::exchange(other.leafs_, 0)},
        new_node_ix_{std::exchange(other.new_node_ix_, 0)},
        new_leaf_ix_{std::exchange(other.new_leaf_ix_, 0)} {}
  inline auto operator=(map_t&& other) noexcept -> map_t& {
    std::swap(nodes_, other.nodes_);
    std::swap(new_node_ix_, other.new_node_ix_);
    std::swap(new_leaf_ix_, other.new_leaf_ix_);
    return *this;
  }
  ~map_t() noexcept = default;

  /** Create a new shared map */
  static auto create(const shared_object_name_t& name = {}) noexcept -> map_t {
    auto nodes = array_t<index_type, node_t>::create(name);
    auto leafs = array_t<index_type, leaf_t>::create(*(nodes.name()) + "L");
    // initialize the desciption
    new (&nodes[0]) description_t{};
    // The default value will be returned whenever the entry is empty
    new (&leafs[0]) leaf_t{0, value_type{}};
    return map_t{std::move(nodes), std::move(leafs)};
  }

  /** Look up an existing shared map*/
  static auto lookup(const shared_object_name_t& name) noexcept -> map_t {
    auto nodes = array_t<index_type, node_t>::lookup(name);
    auto leafs = array_t<index_type, leaf_t>::lookup(*(nodes.name()) + "L");
    return map_t{std::move(nodes), std::move(leafs)};
  }

  /**
   * Get the name of the map.
   * It returns `nullptr` if the underlying storage wasn't created due to an error.
   */
  [[nodiscard]] inline auto name() const noexcept -> const shared_object_name_t* {
    return nodes_.name();
  }

  [[nodiscard]] auto get(key_type key) const noexcept -> value_type {
    INTERPROCESS_LOG_DEBUG("(%zu) Exec get for key %zu...\n", size_t(get_description().root),
                           size_t(key));
    return get(get_description().root, key);
  }
  void set(key_type key, value_type val) noexcept {
    INTERPROCESS_LOG_DEBUG("Exec set for key %zu...\n", size_t(key));
    auto& desc = get_description();
    set(nodes_.memory_guard(), desc, desc.root, key, val);
    INTERPROCESS_LOG_DEBUG("Exec set for key %zu... done!\n", size_t(key));
  }

 private:
  struct description_t {
    std::atomic<index_type> root{0};
    std::atomic<index_type> num_nodes{kNodesOffset};
    std::atomic<index_type> num_leafs{1};
  };

  struct leaf_t {
    key_type key;
    std::atomic<value_type> value;
  };

  struct node_t {
    std::array<std::atomic<index_type>, 1 << kRadixBase> children{};
    key_type code;

    inline node_t(key_type key, std::uint8_t depth) noexcept
        : code{(key & ~key_type((1 << (kKeySize - depth * kRadixBase)) - 1)) | depth} {
      for (auto& child : children) {
        std::atomic_init(&child, 0);
      }
    }

    [[nodiscard]] inline auto depth() const noexcept -> std::uint8_t { return code & kDepthMask; }

    [[nodiscard]] inline auto match(key_type key) const noexcept -> bool {
      auto shift = kKeySize - depth() * kRadixBase;
      return (key >> shift) == (code >> shift);
    }

    [[nodiscard]] inline auto symbol(key_type key) const noexcept -> symbol_type {
      auto shift = kKeySize - (depth() + 1) * kRadixBase;
      return (key >> shift) & kRadixMask;
    }

    [[nodiscard]] inline auto child(key_type key) const noexcept -> index_type {
      return children[symbol(key)].load(std::memory_order_relaxed);
    }

    [[nodiscard]] inline auto child(key_type key) noexcept -> std::atomic<index_type>& {
      return children[symbol(key)];
    }
  };

  static constexpr std::size_t kBaseTreeSize = 32;
  static constexpr std::size_t kBaseValsSize = 256;
  static constexpr std::size_t kNodesOffset = round_up(sizeof(description_t), sizeof(node_t));
  static constexpr std::uint8_t kIndexShift = sizeof(index_type) * 8 - 1;
  static constexpr index_type kIndexTypeMask = index_type{1} << kIndexShift;
  static constexpr index_type kIndexValueMask = ~kIndexTypeMask;
  static constexpr index_type kUnusedNodeIx = kIndexValueMask;

  array_t<index_type, node_t> nodes_;
  array_t<index_type, leaf_t> leafs_;
  // Due to possible races between inserting threads, `set` function may end up allocating a leaf
  // and a node and not using them. To partially address this problem, this structure tracks the
  // leftover indices in a member variables and reuses them on subsequent calls to `set`. The
  // indices can still get lost when this copy of the struct is destroyed, but the losses are
  // considered tolerable.
  index_type new_leaf_ix_ = 0;  // point to the dummy/default leaf that is already allocated
  index_type new_node_ix_ = kUnusedNodeIx;  // a reserved node id (cannot use a valid ix 0)

  inline map_t(array_t<index_type, node_t>&& nodes, array_t<index_type, leaf_t>&& leafs) noexcept
      : nodes_{std::move(nodes)}, leafs_{std::move(leafs)} {}

  [[nodiscard]] auto get_description() noexcept -> description_t& {
    return *reinterpret_cast<description_t*>(&(nodes_[0]));
  }

  [[nodiscard]] auto get_description() const noexcept -> const description_t& {
    return *reinterpret_cast<const description_t*>(&(nodes_[0]));
  }

  /** Number of symbols matching between the current node and the given key. */
  [[nodiscard]] static inline auto matching_depth(key_type a, key_type b) noexcept -> std::uint8_t {
    auto diff = (a ^ b) | kDepthMask;
    return clz(diff) / kRadixBase;
  }

  [[nodiscard]] auto get(index_type node_ix, key_type key) const noexcept -> value_type {
    if (node_ix == 0) {
      return leafs_[0].value.load(std::memory_order_relaxed);
    } else if ((node_ix & kIndexTypeMask) == 0) {
      auto& leaf = leafs_[node_ix];
      return leaf.key == key ? leaf.value.load(std::memory_order_relaxed)
                             : leafs_[0].value.load(std::memory_order_relaxed);
    } else {
      auto& node = nodes_[node_ix & kIndexValueMask];
      if (!node.match(key)) {
        return leafs_[0].value.load(std::memory_order_relaxed);
      }
      return get(node.child(key), key);
    }
  }

  // Allocate a new leaf.
  // It's out-of-the-tree, so no memory ordering required.
  auto new_leaf(description_t& description, key_type key, value_type val) noexcept -> index_type {
    if (new_leaf_ix_ == 0) {
      new_leaf_ix_ = description.num_leafs.fetch_add(1, std::memory_order_relaxed);
    }
    new (&leafs_[new_leaf_ix_]) leaf_t{key, val};
    return new_leaf_ix_;
  }

  // Allocate a new node and put two children inside.
  // It's out-of-the-tree, so no memory ordering required.
  auto new_node(description_t& description, key_type key_a, index_type ix_a, key_type key_b,
                value_type ix_b) noexcept -> index_type {
    auto d = matching_depth(key_a, key_b);
    if (new_node_ix_ == kUnusedNodeIx) {
      new_node_ix_ = description.num_nodes.fetch_add(1, std::memory_order_relaxed);
    }
    auto new_node = new (&nodes_[new_node_ix_]) node_t{key_a, d};
    new_node->child(key_a).store(ix_a, std::memory_order_relaxed);
    new_node->child(key_b).store(ix_b, std::memory_order_relaxed);
    return new_node_ix_ | kIndexTypeMask;
  }

  void set(std::shared_ptr<growing_blob_t::memory_handle_t> memory_guard,
           description_t& description, std::atomic<index_type>& node_source, key_type key,
           value_type val) noexcept {
    auto node_ix = node_source.load(std::memory_order_relaxed);
    index_type new_ix = 0;
    do {
      if (node_ix == 0) {
        // This is the default value; create a new leaf.
        new_ix = new_leaf(description, key, val);
      } else if ((node_ix & kIndexTypeMask) == 0) {
        // This is a leaf; either replace or insert
        auto& cur_leaf = leafs_[node_ix];
        if (cur_leaf.key == key) {
          // replace
          cur_leaf.value.store(val, std::memory_order_relaxed);
          return;
        } else {
          // add new node
          new_ix =
              new_node(description, cur_leaf.key, node_ix, key, new_leaf(description, key, val));
        }
      } else {
        // This is a node; either go recursive or branch this node.
        auto& cur_node = nodes_[node_ix & kIndexValueMask];
        if (cur_node.match(key)) {
          // protect cur_node.child(key) from vanishing
          auto& refreshed_desc = get_description();
          memory_guard = nodes_.memory_guard();
          // full match: go recursive
          return set(memory_guard, refreshed_desc, cur_node.child(key), key, val);
        } else {
          // partial match: branch the node
          new_ix =
              new_node(description, cur_node.code, node_ix, key, new_leaf(description, key, val));
        }
      }
    } while (!node_source.compare_exchange_strong(node_ix, new_ix, std::memory_order_release,
                                                  std::memory_order_relaxed));
    // By this point in code, either a new leaf or a new node have been surely allocated
    // (the other branches have early return statements).
    // The newly allocated leaf has been inserted for sure, the node could have been inserted as
    // well. I reset the variables here to track this.
    new_leaf_ix_ = 0;
    if ((new_ix & kIndexTypeMask) != 0) {
      new_node_ix_ = kUnusedNodeIx;
    }
  }
};

}  // namespace interprocess
