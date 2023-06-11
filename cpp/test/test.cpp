#include <cstdio>

#include <interprocess/array.hpp>
#include <interprocess/map.hpp>
#include <interprocess/shared_object_name.hpp>

#include <unistd.h>  // sleep
// https://stackoverflow.com/questions/397075/what-is-the-difference-between-exit-and-abort

int main() {
  printf("Array section...\n");
  auto a = interprocess::array_t<int, double>::create();
  printf("Allocated an array...\n");
  a[0] = 5.0;
  a[2] = 52.2;
  a[53] = 233.2;
  printf("Array A@%d = %f\n", 2, a[2]);
  a[1535] = 44.00001;
  printf("Array A@%d = %f\n", 1535, a[1535]);
  auto b = interprocess::array_t<int, double>::lookup(*a.name());
  printf("Array@%d: A = %f, B = %f\n", 1535, a[1535], b[1535]);
  printf("Array@%d: A = %f, B = %f\n", 2, a[2], b[2]);
  b[12] = 12.12;
  b[738] = 738.5;
  b[3333] = 3333.3;
  printf("Array@%d: A = %f, B = %f\n", 1535, a[1535], b[1535]);
  printf("Array@%d: A = %f, B = %f\n", 12, a[12], b[12]);
  printf("Array@%d: A = %f, B = %f\n", 738, a[738], b[738]);
  printf("Array@%d: A = %f, B = %f\n", 3333, a[3333], b[3333]);
  printf("Array@%d: A = %f, B = %f\n", 1535, a[1535], b[1535]);

  auto m = interprocess::map_t<8, std::size_t, float>::create();

  printf("Map: radix = %d, key bits = %d, depth bits = %d, trailing bits = %d\n",
         int(decltype(m)::kRadixBase), int(decltype(m)::kKeySize), int(decltype(m)::kDepthBits),
         int(decltype(m)::kTrailingBits));

  for (int i = 0; i < 10; i++) {
    printf("%d: %s\n", i, interprocess::shared_object_name_t{}.value.data());
  }

  m.set(3, 15.3);
  m.set(3257, 42);
  m.set(884545600003257, 0.77);
  m.set(3256, 42);
  printf("Map value: %f\n", m.get(3));
  printf("Map value: %f\n", m.get(3257));
  printf("Map value: %f\n", m.get(3258));
  size_t start = 1002;
  size_t step = 1242;
  for (auto i = start; i < start + step * 10; i += step) {
    m.set(i, float(i));
    printf("%zu - %f\n", i, m.get(i));
  }

  printf("Full scan\n");
  auto q = interprocess::map_t<8, std::size_t, float>::lookup(*m.name());
  int found_count = 0;
  for (int i = 0; i < 1000000; i++) {
    // printf("[A] i: %d \n", i);
    auto r = m.get(i);
    // printf("[B] i: %d \n", i);
    if (r != 0.0f) {
      found_count++;
      printf("[C] %d: %f == %f\n", i, r, q.get(i));
    }
  }
  printf("...found %d records in the range.\n", found_count);

  return 0;
}