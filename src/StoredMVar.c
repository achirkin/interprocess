#include "HsFFI.h"
#include <stdlib.h>
#include <fcntl.h>
#include <semaphore.h>
#include <string.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <unistd.h>
#include <pthread.h>
#include <stdio.h>
#include <time.h>
#include <errno.h>
#include <signal.h>
#include <unistd.h>
#include <limits.h>


#define SHARED_OBJECT_NAME_LENGTH 32


typedef char SharedObjectName[SHARED_OBJECT_NAME_LENGTH];
void genSharedObjectName(char * const name);

typedef struct MVar MVar;

MVar *mvar_new(size_t byteSize);
MVar *mvar_lookup(const char *name, int mvarId);
void  mvar_destroy(MVar *mvar);
void  mvar_name(MVar *mvar, char * const name);

int  mvar_take(MVar *mvar, void *localDataPtr, int takeId);
int  mvar_put(MVar *mvar, void *localDataPtr, int opId);



typedef struct MVarState {
  int                 isFull;
  int                 pendingReaders;
  sem_t               totalUsers;
  pthread_mutex_t     mvMut;
  pthread_mutexattr_t mvMAttr;
  pthread_cond_t      canPutC;
  pthread_cond_t      canTakeC;
  pthread_condattr_t  condAttr;
  size_t              dataSize;
} MVarState;

typedef struct MVar {
  /* State is stored in the shared data area, accessed by all threads.
     It has a fixed size and kept at the beginning of a shared memory region.
   */
  MVarState *statePtr;
  /* Actual data is stored next to the MVarState
   */
  void      *dataPtr;
  /* Name of the shared memory region, used to share mvar across processes.
   */
  SharedObjectName  mvarName;
  int        mvarId;
} MVar;

// ensure 64 byte alignment (maximum possible we can think of, e.g. AVX512)
size_t mvar_state_size64() {
  size_t x = sizeof(MVarState);
  size_t r = x % 64;
  return (r == 0 ? x : (x + 64 - r)) + 256;
}

HsPtr _store_alloc(const char *memBlockName, size_t size) {
  int memFd = shm_open(memBlockName, O_CREAT | O_RDWR, S_IRWXU);
  if (memFd < 0) {
    return NULL;
  }
  int res = ftruncate(memFd, size);
  if (res != 0) {
    shm_unlink(memBlockName);
    return NULL;
  }
  HsPtr r = mmap( NULL
                , size
                , PROT_READ | PROT_WRITE | PROT_EXEC
                , MAP_SHARED
                , memFd, 0);
  if (r == MAP_FAILED) {
    shm_unlink(memBlockName);
    return NULL;
  }
  return r;
}

MVar *mvar_new(size_t byteSize) {
  size_t dataShift = mvar_state_size64();
  size_t totalSize = dataShift + byteSize;
  MVar *r = malloc(sizeof(MVar));
  if (r == NULL) {
    return NULL;
  }
  genSharedObjectName(r->mvarName);

  // allocate memory
  r->statePtr = (MVarState*) _store_alloc(r->mvarName, totalSize);
  if (r->statePtr == NULL) {
    free(r);
    return NULL;
  }
  r->dataPtr = ((void*)(r->statePtr)) + dataShift;
  r->mvarId  = 0;

  // setup state
  MVarState s = (struct MVarState)
    { .isFull = 0
    , .pendingReaders = 0
    , .dataSize = byteSize
    };
  *(r->statePtr) = s;

  printf("new - init...\n");
  int r0 = sem_init(&(s.totalUsers), 1, 0);
  if ( r0 != 0 ) {
    printf("new - error, %d\n", r0);
    munmap(&s, totalSize);
    shm_unlink(r->mvarName);
    free(r);
    return NULL;
  }

  int c = pthread_mutexattr_init(&(s.mvMAttr));
  if ( c != 0 )
    printf( "new pthread_mutexattr_init(&(s.mvMAttr)) broke, %d\n", c );
#ifndef NDEBUG
  c = pthread_mutexattr_settype(&(s.mvMAttr), PTHREAD_MUTEX_ERRORCHECK);
  if ( c != 0 )
    printf( "new pthread_mutexattr_settype(&(s.mvMAttr) broke, %d\n", c );
#endif
  // pthread_mutexattr_settype(&(s.mvMAttr), PTHREAD_MUTEX_ROBUST);
  c = pthread_mutexattr_setpshared(&(s.mvMAttr), PTHREAD_PROCESS_SHARED);
  if ( c != 0 )
    printf( "new pthread_mutexattr_setpshared(&(s.mvMAttr) broke, %d\n", c );
  c = pthread_mutex_init(&(s.mvMut), &(s.mvMAttr));
  if ( c != 0 )
    printf( "new pthread_mutex_init(&(s.mvMut), &(s.mvMAttr)) broke, %d\n", c );
  c = pthread_condattr_init(&(s.condAttr));
  if ( c != 0 )
    printf( "new pthread_condattr_init(&(s.condAttr)) broke, %d\n", c );
  c = pthread_condattr_setpshared(&(s.condAttr), PTHREAD_PROCESS_SHARED);
  if ( c != 0 )
    printf( "new pthread_condattr_setpshared(&(s.condAttr) broke, %d\n", c );
  c = pthread_cond_init(&(s.canPutC), &(s.condAttr));
  if ( c != 0 )
    printf( "new pthread_cond_init(&(s.canPutC) broke, %d\n", c );
  c = pthread_cond_init(&(s.canTakeC), &(s.condAttr));
  if ( c != 0 )
    printf( "new pthread_cond_init(&(s.canTakeC) broke, %d\n", c );
  printf("new - finished!\n");

  return r;
}

MVar *mvar_lookup(const char *name, int givenId) {
  int memFd = shm_open(name, O_RDWR, S_IRWXU);
  if (memFd < 0) {
    printf("[%d] lookup - shm_open failed, errno = %d\n", givenId, errno);
    return NULL;
  }
  // first, map only sizeof(MVarState) bytes
  // then, read the actual size and remap memory
  HsPtr m0 = mmap( NULL
                 , sizeof(MVarState)
                 , PROT_READ | PROT_WRITE | PROT_EXEC
                 , MAP_SHARED
                 , memFd, 0);
  if (m0 == MAP_FAILED) {
    printf("[%d] lookup - map m0 failed, errno = %d\n", givenId, errno);
    return NULL;
  }
  size_t dataShift = mvar_state_size64(),
         storeSize = dataShift + ((MVarState*)m0)->dataSize;
  // HsPtr m = mremap(m0, 0, storeSize, MREMAP_MAYMOVE);
  // if (m == MAP_FAILED) {
  //   munmap(m0, sizeof(MVarState));
  //   return NULL;
  // }
  int r = munmap(m0, sizeof(MVarState));
  if ( r != 0 ) {
    printf("[%d] lookup - munmap m0 failed, r = %d, errno = %d\n", givenId, r, errno);
    return NULL;
  }
  HsPtr m = mmap( NULL
                , storeSize
                , PROT_READ | PROT_WRITE | PROT_EXEC
                , MAP_SHARED
                , memFd, 0);
  if (m == MAP_FAILED) {
    printf("[%d] lookup -map m failed, errno = %d\n", givenId, errno);
    return NULL;
  }
  printf("[%d] lookup - mapped memory, r = %d, errno = %d, size = %ld\n", givenId, r, errno, storeSize);
  // setup MVar struct
  MVar *mvar = malloc(sizeof(MVar));
  if (mvar == NULL) {
    printf("[%d] lookup - malloc mvar failed, errno = %d\n", givenId, errno);
    munmap(m, storeSize);
    return NULL;
  }
  mvar->statePtr = (MVarState*)m;
  mvar->dataPtr  = ((void*)(mvar->statePtr)) + dataShift;
  mvar->mvarId = givenId;
  strcpy(mvar->mvarName, name);
  // update state
  r = sem_post(&(mvar->statePtr->totalUsers));
  if ( r != 0 ) {
    printf("[%d] lookup - sem_post failed, r = %d, errno = %d\n", givenId, r, errno);
    munmap(m, storeSize);
    free(mvar);
    return NULL;
  }
  printf("[%d] lookup - success; errno: %d; name: %s.\n", mvar->mvarId, errno, mvar->mvarName);
  return mvar;
}

void mvar_destroy(MVar *mvar) {
  printf("[%d] destroy - starting...\n", mvar->mvarId);
  size_t storeSize = mvar->statePtr->dataSize + mvar_state_size64();
  if ( sem_trywait(&(mvar->statePtr->totalUsers)) == 0 ) {
    int value;
    sem_getvalue(&(mvar->statePtr->totalUsers), &value);
    printf("[%d] destroy... some users left %d\n", mvar->mvarId, value + 1);
    munmap(mvar->statePtr, storeSize);
  } else {
    if (errno == EAGAIN) {
      printf("[%d] destroy... last one\n", mvar->mvarId);
      int r = pthread_cond_destroy(&(mvar->statePtr->canTakeC));
      if ( r != 0 )
        printf( "pthread_cond_destroy TakeC broke, %d\n", r );
      r = pthread_cond_destroy(&(mvar->statePtr->canPutC));
      if ( r != 0 )
        printf( "pthread_cond_destroy PutC broke, %d\n", r );
      r = pthread_condattr_destroy(&(mvar->statePtr->condAttr));
      if ( r != 0 )
        printf( "pthread_cond_destroy broke, %d\n", r );
      r = pthread_mutex_destroy(&(mvar->statePtr->mvMut));
      if ( r != 0 )
        printf( "pthread_mutex_destroy broke, %d\n", r );
      r = pthread_mutexattr_destroy(&(mvar->statePtr->mvMAttr));
      if ( r != 0 )
        printf( "pthread_mutexattr_destroy broke, %d\n", r );
      r = sem_destroy(&(mvar->statePtr->totalUsers));
      if ( r != 0 )
        printf( "sem_destroy broke, %d\n", r );
      r = munmap(mvar->statePtr, storeSize);
      if ( r != 0 )
        printf( "munmap broke, %d\n", r );
      r = shm_unlink(mvar->mvarName);
      if ( r != 0 )
        printf( "shm_unlink(mvar->mvarName) broke, %d\n", r );
    } else {
      printf("destroy semaphore error DANGER!");
    }
  }
  free(mvar);
  printf("[%d] destroy... finished\n", mvar->mvarId);
}

static const long defWaitTimeInMs = 50;
static void setTimeToWait(struct timespec * const ttwPtr, long timeInMs) {
  clock_gettime(CLOCK_REALTIME, ttwPtr);
  ttwPtr->tv_nsec += 1000000UL * (timeInMs % 1000);
  ttwPtr->tv_sec  += timeInMs / 1000 + ttwPtr->tv_nsec / 1000000000UL;
  ttwPtr->tv_nsec %= 1000000000UL;
}


int mvar_take(MVar *mvar, void *localDataPtr, int opId) {
  long timeInMs = defWaitTimeInMs;
  struct timespec timeToWait;
  printf( "[%d] mvar_take %d entering, errno %d\n", mvar->mvarId, opId, errno);
  int r = 0;
  do {
    setTimeToWait(&timeToWait, timeInMs);
    r = pthread_mutex_timedlock(&(mvar->statePtr->mvMut), &timeToWait);
    if (r == ETIMEDOUT) {
      printf( "[%d] mvar_take %d - pthread_mutex_lock timeout %ld\n", mvar->mvarId, opId, timeInMs);
      timeInMs <<= 1;
    }
  } while ( r == ETIMEDOUT );
  if ( r != 0 ) {
    printf("[%d] mvar_take %d - pthread_mutex_lock errorcode %d\n", mvar->mvarId, opId, r);
    return 1;
  }
  printf( "[%d] mvar_take %d locked, isFull: %d, errno %d\n", mvar->mvarId, opId, mvar->statePtr->isFull, errno);
  timeInMs = defWaitTimeInMs;
  while ( !(mvar->statePtr->isFull) ) {
    r = pthread_cond_signal(&(mvar->statePtr->canPutC));
    if ( r != 0 ) {
      printf("mvar_take - pthread_cond_signal errorcode %d\n", r);
      pthread_mutex_unlock(&(mvar->statePtr->mvMut));
      return 1;
    }
    setTimeToWait(&timeToWait, timeInMs);
    printf( "[%d] mvar_take %d will wait %ld, isFull %d, errno %d\n", mvar->mvarId, opId, timeInMs, mvar->statePtr->isFull, errno);
    r = pthread_cond_timedwait(&(mvar->statePtr->canTakeC), &(mvar->statePtr->mvMut), &timeToWait);
    printf( "[%d] mvar_take %d wait done %ld, isFull %d, r = %d, errno %d\n", mvar->mvarId, opId, timeInMs, mvar->statePtr->isFull, r, errno);
    if ( r == ETIMEDOUT ) {
      printf( "[%d] mvar_take %d wait timeout %ld, isFull %d\n", mvar->mvarId, opId, timeInMs, mvar->statePtr->isFull);
      timeInMs <<= 1;
      r = 0;
    }
    if ( r != 0 ) {
      printf( "[%d] mvar_take %d - pthread_cond_wait returned %d, errno %d\n", mvar->mvarId, opId, r, errno);
      pthread_mutex_unlock(&(mvar->statePtr->mvMut));
      return 1;
    }
  }
  printf( "[%d] mvar_take %d copying\n", mvar->mvarId, opId);
  // copy data
  memcpy(localDataPtr, mvar->dataPtr, mvar->statePtr->dataSize);
  // mark full
  mvar->statePtr->isFull = 0;
  r = pthread_cond_signal(&(mvar->statePtr->canPutC));
  if ( r != 0 ) {
    printf("mvar_take - pthread_cond_broadcast errorcode %d\n", r);
    pthread_mutex_unlock(&(mvar->statePtr->mvMut));
    return 1;
  }
  r = pthread_mutex_unlock(&(mvar->statePtr->mvMut));
  if ( r != 0 ) {
    printf("mvar_take - pthread_mutex_unlock errorcode %d\n", r);
    return 1;
  }
  printf( "[%d] mvar_take %d finished\n", mvar->mvarId, opId);
  return 0;
}


int mvar_put(MVar *mvar, void *localDataPtr, int opId) {
  long timeInMs = defWaitTimeInMs;
  struct timespec timeToWait;
  int r = 0;
  do {
    setTimeToWait(&timeToWait, timeInMs);
    r = pthread_mutex_timedlock(&(mvar->statePtr->mvMut), &timeToWait);
    if (r == ETIMEDOUT) {
      printf( "[%d] mvar_put %d - pthread_mutex_lock timeout %ld\n", mvar->mvarId, opId, timeInMs);
      timeInMs <<= 1;
    }
  } while ( r == ETIMEDOUT );
  if ( r != 0 ) {
    printf("[%d] mvar_put %d - pthread_mutex_lock errorcode %d\n", mvar->mvarId, opId, r);
    return 1;
  }
  printf( "[%d] mvar_put %d locked, isFull: %d\n", mvar->mvarId, opId, mvar->statePtr->isFull);
  timeInMs = defWaitTimeInMs;
  while ( mvar->statePtr->isFull ) {
    r = pthread_cond_signal(&(mvar->statePtr->canTakeC));
    if ( r != 0 ) {
      printf("mvar_put - pthread_cond_signal errorcode %d\n", r);
      pthread_mutex_unlock(&(mvar->statePtr->mvMut));
      return 1;
    }
    setTimeToWait(&timeToWait, timeInMs);
    printf( "[%d] mvar_put %d will wait %ld, isFull %d, errno %d\n", mvar->mvarId, opId, timeInMs, mvar->statePtr->isFull, errno);
    r = pthread_cond_timedwait(&(mvar->statePtr->canPutC), &(mvar->statePtr->mvMut), &timeToWait);
    printf( "[%d] mvar_put %d wait done %ld, isFull %d, r = %d, errno %d\n", mvar->mvarId, opId, timeInMs, mvar->statePtr->isFull, r, errno);
    if ( r == ETIMEDOUT ) {
      printf( "[%d] mvar_put %d wait timeout %ld, isFull %d\n", mvar->mvarId, opId, timeInMs, mvar->statePtr->isFull);
      timeInMs <<= 1;
      r = 0;
    }
    if ( r != 0 ) {
      printf( "[%d] mvar_put %d - pthread_cond_wait returned %d, errno %d\n", mvar->mvarId, opId, r, errno);
      pthread_mutex_unlock(&(mvar->statePtr->mvMut));
      return 1;
    }
  }
  printf( "[%d] mvar_put %d copying\n", mvar->mvarId, opId);
  // copy data
  memcpy(mvar->dataPtr, localDataPtr, mvar->statePtr->dataSize);
  // mark full
  mvar->statePtr->isFull = 1;
  r = pthread_cond_signal(&(mvar->statePtr->canTakeC));
  if ( r != 0 ) {
    printf("mvar_put - pthread_cond_broadcast errorcode %d\n", r);
    pthread_mutex_unlock(&(mvar->statePtr->mvMut));
    return 1;
  }
  r = pthread_mutex_unlock(&(mvar->statePtr->mvMut));
  if ( r != 0 ) {
    printf("mvar_put - pthread_mutex_unlock errorcode %d\n", r);
    return 1;
  }
  printf( "[%d] mvar_put %d finished\n", mvar->mvarId, opId);
  return 0;
}



void mvar_name(MVar *mvar, char * const name) {
  strcpy(name, mvar->mvarName);
}




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
    _my_pid = (int)getpid();
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
  while( (c = c / keytableLength) > 0 && i < (SHARED_OBJECT_NAME_LENGTH-3) ) {
    randV = div(randV.quot, keytableLength);
    globV = div(globV.quot, keytableLength);
    procV = div(procV.quot, keytableLength);
    name[i++] = keytable[randV.rem];
    name[i++] = keytable[globV.rem];
    name[i++] = keytable[procV.rem];
  }
}
