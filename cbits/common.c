#include "common.h"

#include <limits.h>
#include <stdatomic.h>
#include <string.h>
#include <time.h>

#if defined(WIN32) || defined(_WIN32) || defined(__WIN32) || \
    defined(mingw32_HOST_OS)
#include <windows.h>
#define GetMyPid abs((int)GetCurrentProcessId())
#else
#include <unistd.h>
#define GetMyPid (int)getpid()
#endif

static atomic_int process_seed = 0;
static _Thread_local bool initialized = false;
static _Thread_local int process_id = 0;
static const char keytable[] =
    "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz";
static const char prefix[] = "/HsIPC.";
#define keytableLength 62
#define prefixLength 7
// Leave a few symbol placeholders empty to be able to append some
// short suffixes to the object name in-place.
#define suffixLength 1

void genSharedObjectName(char* const name) {
  int seed = atomic_fetch_add_explicit(&process_seed, 1, memory_order_relaxed);
  // Init this once per thread
  if (!initialized) {
    initialized = true;
    srand(time(NULL));
    process_id = GetMyPid;
  }
  // Clear the name
  memset(name, 0, sizeof(SharedObjectName));
  memcpy(name, prefix, sizeof(prefix));
  // Combine three sources of uniqueness
  const int rands_num = 3;
  div_t rands[] = {{.quot = rand() ^ 0x19a628f6, .rem = 0},
                   {.quot = process_id ^ 0x003772a1, .rem = 0},
                   {.quot = seed ^ 0x028ab100, .rem = 0}};
  for (int i = prefixLength, j = 0, k = 0;
       i < SHARED_OBJECT_NAME_LENGTH - 1 - suffixLength && k < rands_num;
       i++, j = (j + 1) % rands_num) {
    rands[j] = div(rands[j].quot, keytableLength);
    name[i] = keytable[rands[j].rem];
    if (rands[j].quot == 0) k++;
  }
}

bool has_blocked_exceptions(StgStablePtr tso) {
  return (typeof(END_TSO_QUEUE))(
             ((StgTSO*)deRefStablePtr(tso))->blocked_exceptions) !=
         END_TSO_QUEUE;
}