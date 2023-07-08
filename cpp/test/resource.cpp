#include <interprocess/resource.hpp>

#include <gtest/gtest.h>

#include <vector>

struct chunk_t {
  double a;
  int64_t b;
};

TEST(resource, basic_alloc_loop) {
  auto res = interprocess::resource_t<chunk_t>::create();
  std::size_t n = 7;
  std::vector<chunk_t*> ptrs{n};
  for (size_t i = 1; i < n; i++) {
    auto* ptr = res.allocate(i);
    ptr->a = i;
    ptr->b = -i;
    ptrs[i] = ptr;
  }
  for (size_t i = 0; i < n; i++) {
    auto* ptr = ptrs[i];
    EXPECT_EQ(ptr->a, i);
    EXPECT_EQ(ptr->b, -i);
    res.deallocate(ptr, i);
  }
}

TEST(resource, basic_alloc_interleaved) {
  auto res = interprocess::resource_t<chunk_t>::create();
  std::size_t n = 7;
  std::vector<chunk_t*> ptrs{n + 1};
  ptrs[0] = res.allocate(1);
  ptrs[0]->a = 1;
  ptrs[0]->b = -1;
  for (size_t i = 1; i <= n; i++) {
    auto* ptr = res.allocate(i);
    ptr->a = i;
    ptr->b = -i;
    ptrs[i] = ptr;
    ptr = ptrs[i - 1];
    EXPECT_EQ(ptr->a, i - 1);
    EXPECT_EQ(ptr->b, 1 - i);
    res.deallocate(ptr, i - 1);
  }
  res.deallocate(ptrs[n], n);
}