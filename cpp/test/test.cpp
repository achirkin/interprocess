#include <cstdio>

#include <interprocess/map.hpp>
#include <interprocess/shared_blob.hpp>
#include <interprocess/shared_object_name.hpp>

//#include <unistd.h>  // sleep
// https://stackoverflow.com/questions/397075/what-is-the-difference-between-exit-and-abort

int main() {
  interprocess::shared_blob_t sb0{50};
  interprocess::shared_blob_t sb1{*sb0.name()};
  interprocess::shared_blob_t sb2{sb1};

  interprocess::map_t<8, std::size_t, float> m{};

  static_assert(interprocess::is_reinterpretable_v<int, void*>);

  printf("Map: radix = %d, key bits = %d, depth bits = %d, trailing bits = %d\n",
         int(decltype(m)::kRadixBase), int(decltype(m)::kKeySize), int(decltype(m)::kDepthBits),
         int(decltype(m)::kTrailingBits));

  printf("Hello world: %s\n", sb0.name()->value.data());
  reinterpret_cast<int*>(sb0.data())[0] = 42;
  // sleep(10);

  for (int i = 0; i < 10; i++) {
    printf("%d: %s\n", i, interprocess::shared_object_name_t{}.value.data());
  }
  printf("Answer 1: %d\n", reinterpret_cast<int*>(sb1.data())[0]);
  printf("Answer 2: %d\n", reinterpret_cast<int*>(sb2.data())[0]);
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

  // printf("Full scan\n");
  // int found_count = 0;
  // for (int i = 0; i < 1000000; i++) {
  //   // printf("[A] i: %d \n", i);
  //   auto r = m.get(i);
  //   // printf("[B] i: %d \n", i);
  //   if (r != 0.0f) {
  //     found_count++;
  //     printf("[C] %d - %f\n", i, r);
  //   }
  // }
  // printf("...found %d records in the range.\n", found_count);

  return 0;
}