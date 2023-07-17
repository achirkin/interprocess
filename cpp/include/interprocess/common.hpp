#pragma once

#include <climits>
#include <cstdint>
#include <type_traits>

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
constexpr inline auto round_up(T number_to_round, T modulus) noexcept -> T {
  auto x = number_to_round + modulus - 1;
  return x - (x % modulus);
}
template <typename T>
constexpr inline auto div_rounding_up(T x, T y) noexcept -> T {
  return (x + y - 1) / y - 1;
}

template <typename T, typename = std::enable_if_t<std::is_integral_v<T>>>
constexpr inline auto is_a_power_of_two(T x) noexcept -> bool {
  return (x != 0) && (((x - 1) & x) == 0);
}

/** Count the minimum number of bits required to represent the given value. */
template <typename T, typename = std::enable_if_t<std::is_integral_v<T> || std::is_pointer_v<T>>>
constexpr inline auto value_bitsize(T x) noexcept -> std::uint8_t {
  std::uint8_t count = 0;
  while (x > 0) {  // NOLINT
    count++;
    x >>= 1;
  }
  return count;
}

/** Count the number of leading zero bits. */
template <typename T, typename = std::enable_if_t<std::is_integral_v<T> || std::is_pointer_v<T>>>
inline auto clz(T x) noexcept -> std::uint8_t {
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

/** Greatest common divisor. */
template <typename T, typename = std::enable_if_t<std::is_integral_v<T>>>
constexpr inline auto gcd(T a, T b) noexcept -> T {
  while (b != 0) {
    auto t = b;
    b = a % b;
    a = t;
  }
  return a;
}

/** Least common multiple. */
template <typename T, typename = std::enable_if_t<std::is_integral_v<T>>>
constexpr inline auto lcm(T a, T b) noexcept -> T {
  return (a / gcd(a, b)) * b;
}

namespace {

template <std::uint8_t bits>
struct uint {
  using type = void;
};  // NOLINT

template <>
struct uint<8> {  // NOLINT
  using type = std::uint8_t;
};

template <>
struct uint<16> {  // NOLINT
  using type = std::uint16_t;
};

template <>
struct uint<32> {  // NOLINT
  using type = std::uint32_t;
};

template <>
struct uint<64> {  // NOLINT
  using type = std::uint64_t;
};

#if defined(__SIZEOF_INT128__)
template <>
struct uint<128> {  // NOLINT
  using type = __uint128_t;
};
#endif

template <std::uint8_t value_bits, std::uint8_t bits = 8>  // NOLINT
struct smallest_fitting_uint {
  // NB: I rely on bits being a uint8_t and wrap to zero.
  using type = std::conditional_t<value_bits <= bits, typename uint<bits>::type,
                                  typename smallest_fitting_uint<value_bits, (bits << 1)>::type>;
};

template <std::uint8_t value_bits>
struct smallest_fitting_uint<value_bits, 0> {
  using type = void;
};

}  // namespace

/** Unsigned integer of a fixed bit-width. */
template <std::uint8_t bits>
using uint_t = typename uint<bits>::type;

/** Smallest unsigned integer that fits the given number of bits. */
template <std::uint8_t value_bits>
using smallest_fitting_uint_t = typename smallest_fitting_uint<value_bits>::type;

}  // namespace interprocess
