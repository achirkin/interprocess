#define INTERPROCESS_STRUCT_INSPECT

#include <interprocess/resource.hpp>

#include <gtest/gtest.h>

#include <iostream>
#include <random>
#include <thread>
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
      auto tag = (val & resource_t<T>::kTagMask) >> resource_t<T>::kPtrBits;
      auto idx = (val & resource_t<T>::kPtrMask);
      return out << "{" << tag << "|" << idx << "; " << node.size << "}";
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
  auto view_sizes() {
    return print_stream{[&](std::ostream& out) -> std::ostream& {
      EXPECT_LE(res.size_ * sizeof(T), res.blob_.grow(0));
      return out << "blob: " << res.blob_.grow(0) << "; size: " << res.size_ << " * " << sizeof(T);
    }};
  }
  template <typename S>
  auto view_ptr(const S* ptr) {
    return print_stream{[&, ptr](std::ostream& out) -> std::ostream& {
      auto offset = reinterpret_cast<const std::byte*>(ptr) -
                    reinterpret_cast<const std::byte*>(res.blob_.data());
      return out << "ptr: " << ptr << "; offset: " << offset << "; ix: " << (offset / sizeof(T));
    }};
  }

  auto count_visits() {
    index_t counter = 0;
    visit_nodes(resource_t<T>::kRoot, [&counter](const auto& node, index_t, bool) {
      counter += (node.next_idx.load() & resource_t<T>::kTagMask) >> resource_t<T>::kPtrBits;
    });
    return counter;
  }

  auto count_total_size() {
    auto* root = res.get(resource_t<T>::kRoot);
    index_t total_size = 1;  // root node
    visit_nodes(root->next_idx.load(),
                [&total_size](const auto& node, index_t, bool) { total_size += node.size; });
    return total_size;
  }

  auto get_total_size() { return res.get(resource_t<T>::kRoot)->size.load(); }

  void check_memory_leaks() {
    auto* root = res.get(resource_t<T>::kRoot);
    index_t total_size = 1;  // root node
    visit_nodes(root->next_idx.load(),
                [&total_size](const auto& node, index_t, bool) { total_size += node.size; });
    ASSERT_EQ(total_size, root->size.load()) << view_all();
  }
};

}  // namespace interprocess

#define CHECK_VISIT_LEAKS(inspect) ASSERT_EQ(inspect.count_visits(), 0) << inspect.view_shared()
#define CHECK_MEMORY_LEAKS(inspect) \
  ASSERT_EQ(inspect.count_total_size(), inspect.get_total_size()) << inspect.view_all()

inline constexpr size_t kSmallN = 7;

struct chunk_t {
  double a;
  int64_t b;
};

// NOLINTNEXTLINE
TEST(resource, basic_alloc_loop) {
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
    CHECK_VISIT_LEAKS(inspect);
  }
  for (size_t i = 0; i < kSmallN; i++) {
    auto [ptr, elems] = allocs[i];
    for (size_t j = 0; j < elems; j++) {
      EXPECT_EQ(ptr[j].a, double(i));
      EXPECT_EQ(ptr[j].b, -int64_t(i));
    }
    res.deallocate(ptr, elems);
    CHECK_VISIT_LEAKS(inspect);
  }
  CHECK_MEMORY_LEAKS(inspect);
}

// NOLINTNEXTLINE
TEST(resource, basic_alloc_interleaved_a) {
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
      CHECK_VISIT_LEAKS(inspect);
    }
    if (i > 0) {
      auto [ptr, elems] = allocs[i - 1];
      for (size_t j = 0; j < elems; j++) {
        EXPECT_EQ(ptr[j].a, double(i - 1));
        EXPECT_EQ(ptr[j].b, -int64_t(i - 1));
      }
      res.deallocate(ptr, elems);
    }
    CHECK_VISIT_LEAKS(inspect);
  }
  CHECK_MEMORY_LEAKS(inspect);
}

// NOLINTNEXTLINE
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
    CHECK_VISIT_LEAKS(inspect);
    if (i > 0) {
      auto [ptr, elems] = allocs[i - 1];
      for (size_t j = 0; j < elems; j++) {
        EXPECT_EQ(ptr[j].a, double(i - 1));
        EXPECT_EQ(ptr[j].b, -int64_t(i - 1));
      }
      res.deallocate(ptr, elems);
    }
    CHECK_VISIT_LEAKS(inspect);
  }
  CHECK_MEMORY_LEAKS(inspect);
}

template <bool RuntimeChecks>
void random_interleaved_allocs(interprocess::resource_t<chunk_t> res, uint32_t seed, size_t len,
                               size_t overlap_len) {
  auto inspect = interprocess::inspect_resource_t{res};
  std::default_random_engine engine(seed);
  std::negative_binomial_distribution<std::size_t> rng(10, 0.7);
  std::vector<std::tuple<chunk_t*, size_t>> allocs{overlap_len};
  for (size_t i = 0; i < len + overlap_len; i++) {
    if (i >= overlap_len) {
      auto [ptr, elems] = allocs[i % overlap_len];
      for (size_t j = 0; j < elems; j++) {
        EXPECT_EQ(ptr[j].a, double(i + j - overlap_len)) << inspect.view_ptr(ptr + j);
        EXPECT_EQ(ptr[j].b, -int64_t(i + j - overlap_len)) << inspect.view_ptr(ptr + j);
      }
      res.deallocate(ptr, elems);
    }
    if constexpr (RuntimeChecks) {
      CHECK_VISIT_LEAKS(inspect);
    }
    if (i < len) {
      size_t elems = rng(engine);
      auto* ptr = res.allocate(elems);
      allocs[i % overlap_len] = std::make_tuple(ptr, elems);
      for (size_t j = 0; j < elems; j++) {
        ptr[j].a = double(i + j);
        ptr[j].b = -int64_t(i + j);
      }
    }
    if constexpr (RuntimeChecks) {
      CHECK_VISIT_LEAKS(inspect);
    }
  }
  if constexpr (RuntimeChecks) {
    CHECK_MEMORY_LEAKS(inspect);
  }
}

// NOLINTNEXTLINE
TEST(resource, random_alloc_singlethreaded) {
  auto res = interprocess::resource_t<chunk_t>::create();
  random_interleaved_allocs<true>(res, 42, 1000, 100);
}

struct random_allocs_params_t {
  size_t n_allocs;
  size_t n_alloc_threads = 1;
  size_t n_dealloc_threads = 1;
  size_t rng_pam_n = 10;
  double rng_pam_p = 0.7;
  uint32_t seed = 42;

  friend inline auto operator<<(std::ostream& os, const random_allocs_params_t& p)
      -> std::ostream& {
    os << "{n_allocs=" << p.n_allocs;
    os << ", n_alloc_threads=" << p.n_alloc_threads;
    os << ", n_dealloc_threads=" << p.n_dealloc_threads;
    os << ", rng_pam_n=" << p.rng_pam_n;
    os << ", rng_pam_p=" << p.rng_pam_p;
    os << ", seed=" << p.seed << "}";
    return os;
  }
};

class random_allocs_test_t : public ::testing::TestWithParam<random_allocs_params_t> {
 private:
  random_allocs_params_t params_{::testing::TestWithParam<random_allocs_params_t>::GetParam()};
  interprocess::resource_t<chunk_t> res_{interprocess::resource_t<chunk_t>::create()};
  interprocess::inspect_resource_t<chunk_t> inspect_{res_};

  static void alloc_series(interprocess::resource_t<chunk_t> res, size_t len, ptrdiff_t* ptrs,
                           const size_t* sizes) {
    for (size_t i = 0; i < len; i++) {
      auto elems = sizes[i];
      auto* ptr = res.allocate(elems);
      auto offset = res.get_memory_offset(ptr);
      ptrs[i] = offset;
      for (size_t j = 0; j < elems; j++) {
        ptr[j].a = double(offset * 3 + j);
        ptr[j].b = -int64_t(offset * 5 + j);
      }
    }
  }

  static void dealloc_series(interprocess::resource_t<chunk_t> res, size_t len,
                             const ptrdiff_t* ptrs, const size_t* sizes) {
    auto inspect = interprocess::inspect_resource_t{res};
    for (size_t i = 0; i < len; i++) {
      auto offset = ptrs[i];
      auto ptr = res.from_memory_offset(ptrs[i]);
      auto elems = sizes[i];
      for (size_t j = 0; j < elems; j++) {
        EXPECT_EQ(ptr[j].a, double(offset * 3 + j)) << inspect.view_ptr(ptr + j);
        EXPECT_EQ(ptr[j].b, -int64_t(offset * 5 + j)) << inspect.view_ptr(ptr + j);
      }
      res.deallocate(ptr, elems);
    }
  }

 public:
  // NOLINTNEXTLINE
  void test_batched_allocs() {
    std::default_random_engine engine(params_.seed);
    std::negative_binomial_distribution<std::size_t> rng(params_.rng_pam_n, params_.rng_pam_p);
    std::vector<ptrdiff_t> ptrs;
    std::vector<size_t> sizes;
    ptrs.resize(params_.n_allocs);
    sizes.resize(params_.n_allocs);
    for (auto& size : sizes) {
      size = rng(engine);
    }

    {
      std::vector<std::thread> threads{params_.n_alloc_threads};
      auto max_batch_size = interprocess::div_rounding_up(sizes.size(), threads.size());
      for (size_t i = 0; i < threads.size(); i++) {
        auto batch_start = max_batch_size * i;
        auto batch_len = std::min(batch_start + max_batch_size, sizes.size()) - batch_start;
        threads[i] = std::thread(alloc_series, res_, batch_len, ptrs.data() + batch_start,
                                 sizes.data() + batch_start);
      }
      for (auto& thread : threads) {
        thread.join();
      }
    }
    CHECK_VISIT_LEAKS(inspect_);
    {
      std::vector<std::thread> threads{params_.n_dealloc_threads};
      auto max_batch_size = interprocess::div_rounding_up(sizes.size(), threads.size());
      for (size_t i = 0; i < threads.size(); i++) {
        auto batch_start = max_batch_size * i;
        auto batch_len = std::min(batch_start + max_batch_size, sizes.size()) - batch_start;
        threads[i] = std::thread(dealloc_series, res_, batch_len, ptrs.data() + batch_start,
                                 sizes.data() + batch_start);
      }
      for (auto& thread : threads) {
        thread.join();
      }
    }
    CHECK_VISIT_LEAKS(inspect_);
    CHECK_MEMORY_LEAKS(inspect_);
  }
};

// NOLINTNEXTLINE
TEST_P(random_allocs_test_t, batched_allocs) { test_batched_allocs(); }

// NOLINTNEXTLINE
INSTANTIATE_TEST_SUITE_P(
    resource, random_allocs_test_t,
    testing::Values(random_allocs_params_t{10, 1, 1}, random_allocs_params_t{100, 1, 1},
                    random_allocs_params_t{20, 2, 1}, random_allocs_params_t{50, 2, 1},
                    random_allocs_params_t{50, 5, 1}, random_allocs_params_t{500, 20, 1},
                    random_allocs_params_t{20, 1, 1}, random_allocs_params_t{50, 1, 2},
                    random_allocs_params_t{50, 1, 5}, random_allocs_params_t{500, 1, 20},
                    random_allocs_params_t{20, 1, 1}, random_allocs_params_t{50, 2, 2},
                    random_allocs_params_t{50, 5, 5}, random_allocs_params_t{500, 20, 20}));

// NOLINTNEXTLINE
TEST(resource, random_alloc_interleaved_multithreaded) {
  auto res = interprocess::resource_t<chunk_t>::create();
  constexpr size_t kThreads = 2;
  std::seed_seq s;
  std::vector<std::uint32_t> seeds{kThreads};
  std::vector<std::thread> threads{kThreads};
  s.generate(seeds.begin(), seeds.end());
  for (size_t i = 0; i < kThreads; i++) {
    threads[i] = std::thread(random_interleaved_allocs<false>, res, seeds[i], 20, 5);
  }
  for (auto& thread : threads) {
    thread.join();
  }
  auto inspect = interprocess::inspect_resource_t{res};
  CHECK_VISIT_LEAKS(inspect);
  CHECK_MEMORY_LEAKS(inspect);
}
