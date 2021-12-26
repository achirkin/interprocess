#include "common.h"

#include <limits.h>
#include <stdlib.h>
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

static int _unique_seed = 0;
static int _my_pid = 0;
static const char keytable[] =
    "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz";
static const char prefix[] = "/HsIPC.";
#define keytableLength 62
#define prefixLength 7

void genSharedObjectName(char* const name) {
  // Init this once per process
  if (_unique_seed == 0) {
    srand(time(NULL));
    _my_pid = GetMyPid;
  }
  // Clear the name
  memset(name, 0, sizeof(SharedObjectName));
  memcpy(name, prefix, sizeof(prefix));
  // Combine three sources of uniqueness
  const int rands_num = 3;
  div_t rands[] = {{.quot = rand() ^ 0x19a628f6, .rem = 0},
                   {.quot = _my_pid ^ 0x003772a1, .rem = 0},
                   {.quot = (_unique_seed++) ^ 0x028ab100, .rem = 0}};
  for (int i = prefixLength, j = 0, k = 0;
       i < SHARED_OBJECT_NAME_LENGTH - 1 && k < rands_num;
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