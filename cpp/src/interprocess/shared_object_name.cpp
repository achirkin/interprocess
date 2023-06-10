
#include <interprocess/shared_object_name.hpp>

#include <atomic>
#include <ctime>
#include <cstdlib>

#if defined(WIN32) || defined(_WIN32) || defined(__WIN32) || defined(mingw32_HOST_OS)
#include <windows.h>
#define GET_MY_PID() abs((std::int32_t)GetCurrentProcessId())  // NOLINT
#else
#include <unistd.h>
#define GET_MY_PID() (std::int32_t) getpid()  // NOLINT
#endif

namespace interprocess {

shared_object_name_t::shared_object_name_t() noexcept {
  static constexpr std::size_t kKeytableLength = 62;
  static constexpr std::size_t kPrefixLength = 7;

  static const std::array<std::int8_t, kKeytableLength + 1> kKeytable{
      "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"};
  static const std::array<std::int8_t, kPrefixLength + 1> kPrefix{"/HsIPC."};

  static std::atomic<std::int32_t> process_seed{};
  static thread_local bool initialized{false};
  static thread_local std::int32_t process_id{0};

  auto seed = process_seed.fetch_add(1, std::memory_order_relaxed);

  // Init this once per thread
  if (!initialized) {
    initialized = true;
    std::srand(std::time(nullptr));
    process_id = GET_MY_PID();
  }
  // Set the prefix
  std::copy(std::begin(kPrefix), std::end(kPrefix), std::begin(value));
  // Combine three sources of uniqueness
  constexpr std::size_t kRandsNum = 3;
  std::div_t rands[] = {{rand() ^ 0x19a628f6 /* NOLINT */, 0},
                        {process_id ^ 0x003772a1 /* NOLINT */, 0},
                        {seed ^ 0x028ab100 /* NOLINT */, 0}};
  for (std::size_t i = kPrefixLength, j = 0, k = 0;
       i < kNameLength - 1 - kSuffixLength && k < kRandsNum; i++, j = (j + 1) % kRandsNum) {
    rands[j] = div(rands[j].quot, kKeytableLength);
    value[i] = kKeytable[rands[j].rem];
    if (rands[j].quot == 0) {
      k++;
    }
  }
}

}  // namespace interprocess
