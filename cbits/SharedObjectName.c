#include "SharedObjectName.h"
#include <limits.h>
#include <time.h>
#include <stdlib.h>
#include <string.h>

#if defined(WIN32) || defined(_WIN32) || defined(__WIN32) || defined(mingw32_HOST_OS)
#include <windows.h>
#define GetMyPid abs((int)GetCurrentProcessId())
#else
#include <unistd.h>
#define GetMyPid (int)getpid()
#endif

static int _unique_seed = 0;
static int _my_pid = 0;
static const char keytable[]
  = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz";
static const char prefix[]
  = "/HsIPC.";
#define keytableLength 62
#define prefixLength 7


void genSharedObjectName(char * const name) {
  // init this once per process
  if(_unique_seed == 0){
    srand(time(NULL));
    _my_pid = GetMyPid;
  }
  // clear variable
  memset(name, 0, sizeof(SharedObjectName));
  memcpy(name, prefix, sizeof(prefix));
  // put three chars at a time
  int c = INT_MAX, i = prefixLength;
  div_t randV = { .quot = rand()  ^ 0x19a628f6
                , .rem  = 0 }
      , globV = { .quot = _my_pid ^ 0x003772a1
                , .rem  = 0 }
      , procV = { .quot = (_unique_seed++) ^ 0x028ab100
                , .rem  = 0 };
  while( (c = c / keytableLength) > 0 && i < (SharedObjectNameLength-3) ) {
    randV = div(randV.quot, keytableLength);
    globV = div(globV.quot, keytableLength);
    procV = div(procV.quot, keytableLength);
    name[i++] = keytable[randV.rem];
    name[i++] = keytable[globV.rem];
    name[i++] = keytable[procV.rem];
  }
}
