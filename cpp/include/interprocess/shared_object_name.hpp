#include <array>
#include <cstddef>
#include <cstdint>
#include <cstring>  // strcat

#pragma once

namespace interprocess {

/** A globally unique name (unique across processes). */
struct shared_object_name_t {
  /** Number of symbols in the name (including the NULL at the end). */
  static constexpr std::size_t kNameLength = 32;
  /**
   * Leave a few symbol placeholders empty to be able to append some short suffixes to the object
   * name in-place.
   */
  static constexpr std::size_t kSuffixLength = 1;

  /** A C-style string wrapped inside an array. */
  std::array<std::int8_t, kNameLength> value{};

  /** Create a new unique shared object name. */
  shared_object_name_t() noexcept;

  /** Get the name as a C-style string. */
  [[nodiscard]] inline auto c_str() const noexcept -> const char* {
    return reinterpret_cast<const char*>(value.data());
  }

  inline auto operator+(const char* suffix) const noexcept -> shared_object_name_t {
    auto new_name = *this;
    std::strcat(reinterpret_cast<char*>(new_name.value.data()), suffix);
    return new_name;
  }
};

static_assert(sizeof(shared_object_name_t) == shared_object_name_t::kNameLength);

}  // namespace interprocess
