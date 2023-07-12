#define INTERPROCESS_STRUCT_INSPECT

#include <interprocess/resource.hpp>

#include <gtest/gtest.h>

#include <iostream>
#include <tuple>
#include <vector>

namespace interprocess {
template <typename T>
struct inspect_resource_t {
  using index_t = typename resource_t<T>::index_t;
  using node_t = typename resource_t<T>::node_t;
  resource_t<T>& res;

  inspect_resource_t(resource_t<T>& res) : res(res) {}

  static std::ostream& view_node(std::ostream& out, const node_t& node) {
    auto val = node.next_idx.load();
    auto tag = (val & resource_t<T>::kTagMask) >> resource_t<T>::kTagShift;
    auto idx = (val & resource_t<T>::kPtrMask);
    return out << "{" << (tag >> 1) << "|" << (tag & 1) << "|" << idx << "; " << node.size << "}";
  }

  template <typename View>
  void visit_nodes(index_t root_idx, View view) {
    bool first = true;
    while ((root_idx & resource_t<T>::kPtrMask) != resource_t<T>::kTerminal) {
      auto& node = *res.get(root_idx);
      if (first) {
        view(node, root_idx, true);
        first = false;
      } else {
        view(node, root_idx, false);
      }
      root_idx = node.next_idx.load();
    }
  }

  void debug_view(index_t root_idx) {
    visit_nodes(root_idx, [](const auto& node, index_t ix, bool first) {
      if (first) {
        std::cout << ix << ": ";
        view_node(std::cout, node);
      } else {
        std::cout << " -> ";
        view_node(std::cout, node);
      }
    });
    std::cout << std::endl;
  }

  void debug_view_shared() {
    std::cout << "resource.shared: ";
    debug_view(resource_t<T>::kRoot);
  }

  void debug_view_local() {
    std::cout << "resource.local: ";
    debug_view(res.owned_nodes_);
  }

  index_t count_visits() {
    index_t counter = 0;
    visit_nodes(resource_t<T>::kRoot, [&counter](const auto& node, index_t, bool) {
      counter += (node.next_idx.load() & resource_t<T>::kTagMask) >> resource_t<T>::kTagShift;
    });
    return counter;
  }
};

}  // namespace interprocess

inline constexpr size_t kSmallN = 7;

struct chunk_t {
  double a;
  int64_t b;
};

TEST(resource, basic_alloc_loop) {
  auto res = interprocess::resource_t<chunk_t>::create();
  auto inspect = interprocess::inspect_resource_t{res};
  std::vector<std::tuple<chunk_t*, size_t>> allocs{kSmallN};
  for (size_t i = 0; i < kSmallN; i++) {
    size_t elems = 10 + i;
    auto* ptr = res.allocate(elems);
    allocs[i] = std::make_tuple(ptr, elems);
    for (size_t j = 0; j < elems; j++) {
      ptr[j].a = double(i);
      ptr[j].b = -int64_t(i);
    }
    EXPECT_EQ(inspect.count_visits(), 0);
  }
  inspect.debug_view_shared();
  inspect.debug_view_local();
  for (size_t i = 0; i < kSmallN; i++) {
    auto [ptr, elems] = allocs[i];
    for (size_t j = 0; j < elems; j++) {
      EXPECT_EQ(ptr[j].a, double(i));
      EXPECT_EQ(ptr[j].b, -int64_t(i));
    }
    EXPECT_EQ(inspect.count_visits(), 0);
  }
  inspect.debug_view_shared();
  inspect.debug_view_local();
}

TEST(resource, basic_alloc_interleaved_a) {
  auto res = interprocess::resource_t<chunk_t>::create();
  auto inspect = interprocess::inspect_resource_t{res};
  std::vector<std::tuple<chunk_t*, size_t>> allocs{kSmallN};
  for (size_t i = 0; i <= kSmallN; i++) {
    if (i < kSmallN) {
      size_t elems = 10 + i;
      auto* ptr = res.allocate(elems);
      allocs[i] = std::make_tuple(ptr, elems);
      for (size_t j = 0; j < elems; j++) {
        ptr[j].a = double(i);
        ptr[j].b = -int64_t(i);
      }
      EXPECT_EQ(inspect.count_visits(), 0);
    }
    if (i > 0) {
      auto [ptr, elems] = allocs[i - 1];
      for (size_t j = 0; j < elems; j++) {
        EXPECT_EQ(ptr[j].a, double(i - 1));
        EXPECT_EQ(ptr[j].b, -int64_t(i - 1));
      }
      res.deallocate(ptr, elems);
    }
    EXPECT_EQ(inspect.count_visits(), 0);
  }
  inspect.debug_view_shared();
  inspect.debug_view_local();
}

TEST(resource, basic_alloc_interleaved_b) {
  auto res = interprocess::resource_t<chunk_t>::create();
  auto inspect = interprocess::inspect_resource_t{res};
  std::vector<std::tuple<chunk_t*, size_t>> allocs{kSmallN};
  for (size_t i = 0; i <= kSmallN; i++) {
    if (i < kSmallN) {
      size_t elems = kSmallN * 3 - i;
      auto* ptr = res.allocate(elems);
      allocs[i] = std::make_tuple(ptr, elems);
      for (size_t j = 0; j < elems; j++) {
        ptr[j].a = double(i);
        ptr[j].b = -int64_t(i);
      }
    }
    EXPECT_EQ(inspect.count_visits(), 0);
    if (i > 0) {
      auto [ptr, elems] = allocs[i - 1];
      for (size_t j = 0; j < elems; j++) {
        EXPECT_EQ(ptr[j].a, double(i - 1));
        EXPECT_EQ(ptr[j].b, -int64_t(i - 1));
      }
      res.deallocate(ptr, elems);
    }
    EXPECT_EQ(inspect.count_visits(), 0);
  }
  inspect.debug_view_shared();
  inspect.debug_view_local();
}
