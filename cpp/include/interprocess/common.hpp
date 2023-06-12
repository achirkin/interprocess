#include <climits>
#include <cstdint>
#include <type_traits>

#pragma once

#ifdef INTERPROCESS_DEBUG
#include <cstdio>
#define INTERPROCESS_LOG_DEBUG(...) /* NOLINT */       \
  do {                                                 \
    printf("%s:%d ", __FILE__, __LINE__); /* NOLINT */ \
    printf(__VA_ARGS__);                  /* NOLINT */ \
  } while (0)
#else
#define INTERPROCESS_LOG_DEBUG(...) (void)0 /* NOLINT */
#endif

namespace interprocess {

template <typename T>
constexpr inline auto round_up(T number_to_round, T modulus) -> T {
  auto x = number_to_round + modulus - 1;
  return x - (x % modulus);
}
template <typename T>
constexpr inline auto div_rounding_up(T x, T y) -> T {
  return (x + y - 1) / y - 1;
}

/** Count the minimum number of bits required to represent the given value. */
template <typename T, typename = std::enable_if_t<std::is_integral_v<T> || std::is_pointer_v<T>>>
constexpr inline auto value_bitsize(T x) -> std::uint8_t {
  std::uint8_t count = 0;
  while (x > 0) {  // NOLINT
    count++;
    x >>= 1;
  }
  return count;
}

/** Count the number of leading zero bits. */
template <typename T, typename = std::enable_if_t<std::is_integral_v<T> || std::is_pointer_v<T>>>
inline auto clz(T x) -> std::uint8_t {
  if constexpr (sizeof(T) == sizeof(unsigned int)) {
    return std::uint8_t(__builtin_clz((unsigned int)(x)));
  } else if constexpr (sizeof(T) == sizeof(unsigned long)) {
    return std::uint8_t(__builtin_clzl((unsigned long)(x)));
  } else if constexpr (sizeof(T) == sizeof(unsigned long long)) {
    return std::uint8_t(__builtin_clzl((unsigned long long)x));
  } else {
    return sizeof(T) * CHAR_BIT - value_bitsize(x);
  }
}

}  // namespace interprocess
