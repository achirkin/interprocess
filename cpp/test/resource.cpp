#define INTERPROCESS_STRUCT_INSPECT

#include <interprocess/resource.hpp>

#include <gtest/gtest.h>

#include <iostream>
#include <tuple>
#include <vector>

template <typename F>
struct print_stream {
  F f;
  print_stream(F&& f) : f(std::forward<F>(f)) {}
  friend auto operator<<(std::ostream& out, const print_stream& v) -> std::ostream& {
    return v.f(out);
  }
};

namespace interprocess {
template <typename T>
struct inspect_resource_t {
  using index_t = typename resource_t<T>::index_t;
  using node_t = typename resource_t<T>::node_t;
  resource_t<T>& res;

  inspect_resource_t(resource_t<T>& res) : res(res) {}

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

  static auto view_node(const node_t& node) {
    return print_stream{[&node](std::ostream& out) -> std::ostream& {
      auto val = node.next_idx.load();
      auto tag = (val & resource_t<T>::kTagMask) >> resource_t<T>::kTagShift;
      auto idx = (val & resource_t<T>::kPtrMask);
      return out << "{" << (tag >> 1) << "|" << (tag & 1) << "|" << idx << "; " << node.size << "}";
    }};
  }

  auto view_chain(index_t root_idx) {
    return print_stream{[&, root_idx](std::ostream& out) -> std::ostream& {
      visit_nodes(root_idx, [&out](const auto& node, index_t ix, bool first) {
        if (first) {
          out << ix << ": " << view_node(node);
        } else {
          out << " -> " << view_node(node);
        }
      });
      return out;
    }};
  }

  auto view_shared() {
    return print_stream{[&](std::ostream& out) -> std::ostream& {
      return out << "resource.shared: " << view_chain(resource_t<T>::kRoot);
    }};
  }
  auto view_local() {
    return print_stream{[&](std::ostream& out) -> std::ostream& {
      return out << "resource.local: " << view_chain(res.owned_nodes_);
    }};
  }
  auto view_all() {
    return print_stream{[&](std::ostream& out) -> std::ostream& {
      return out << "Current state:" << std::endl
                 << "\t" << view_shared() << std::endl
                 << "\t" << view_local() << std::endl;
    }};
  }

  auto count_visits() {
    index_t counter = 0;
    visit_nodes(resource_t<T>::kRoot, [&counter](const auto& node, index_t, bool) {
      counter += (node.next_idx.load() & resource_t<T>::kTagMask) >> resource_t<T>::kTagShift;
    });
    return counter;
  }

  void check_visit_leaks() { EXPECT_EQ(count_visits(), 0) << view_shared(); }

  void check_memory_leaks() {
    index_t total_size = 2;  // root and terminal nodes
    visit_nodes(resource_t<T>::kRoot,
                [&total_size](const auto& node, index_t, bool) { total_size += node.size; });
    EXPECT_EQ(total_size, res.get(resource_t<T>::kTerminal)->next_idx.load()) << view_all();
  }
};

}  // namespace interprocess

inline constexpr size_t kSmallN = 7;

struct chunk_t {
  double a;
  int64_t b;
};

TEST(resource, basic_alloc_loop) /* NOLINT */ {
  auto res = interprocess::resource_t<chunk_t>::create();
  auto inspect = interprocess::inspect_resource_t{res};
  std::vector<std::tuple<chunk_t*, size_t>> allocs{kSmallN};
  for (size_t i = 0; i < kSmallN; i++) {
    size_t elems = 10 + i;  // NOLINT
    auto* ptr = res.allocate(elems);
    allocs[i] = std::make_tuple(ptr, elems);
    for (size_t j = 0; j < elems; j++) {
      ptr[j].a = double(i);
      ptr[j].b = -int64_t(i);
    }
    inspect.check_visit_leaks();
  }
  for (size_t i = 0; i < kSmallN; i++) {
    auto [ptr, elems] = allocs[i];
    for (size_t j = 0; j < elems; j++) {
      EXPECT_EQ(ptr[j].a, double(i));
      EXPECT_EQ(ptr[j].b, -int64_t(i));
    }
    res.deallocate(ptr, elems);
    inspect.check_visit_leaks();
  }
  inspect.check_memory_leaks();
}

TEST(resource, basic_alloc_interleaved_a) /* NOLINT */ {
  auto res = interprocess::resource_t<chunk_t>::create();
  auto inspect = interprocess::inspect_resource_t{res};
  std::vector<std::tuple<chunk_t*, size_t>> allocs{kSmallN};
  for (size_t i = 0; i <= kSmallN; i++) {
    if (i < kSmallN) {
      size_t elems = 10 + i;  // NOLINT
      auto* ptr = res.allocate(elems);
      allocs[i] = std::make_tuple(ptr, elems);
      for (size_t j = 0; j < elems; j++) {
        ptr[j].a = double(i);
        ptr[j].b = -int64_t(i);
      }
      inspect.check_visit_leaks();
    }
    if (i > 0) {
      auto [ptr, elems] = allocs[i - 1];
      for (size_t j = 0; j < elems; j++) {
        EXPECT_EQ(ptr[j].a, double(i - 1));
        EXPECT_EQ(ptr[j].b, -int64_t(i - 1));
      }
      res.deallocate(ptr, elems);
    }
    inspect.check_visit_leaks();
  }
  inspect.check_memory_leaks();
}

TEST(resource, basic_alloc_interleaved_b) /* NOLINT */ {
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
    inspect.check_visit_leaks();
    if (i > 0) {
      auto [ptr, elems] = allocs[i - 1];
      for (size_t j = 0; j < elems; j++) {
        EXPECT_EQ(ptr[j].a, double(i - 1));
        EXPECT_EQ(ptr[j].b, -int64_t(i - 1));
      }
      res.deallocate(ptr, elems);
    }
    inspect.check_visit_leaks();
  }
  inspect.check_memory_leaks();
}
