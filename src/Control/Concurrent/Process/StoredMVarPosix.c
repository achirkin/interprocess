#include <common.h>

#include <assert.h>
#include <errno.h>
#include <fcntl.h>
#include <pthread.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include <time.h>
#include <unistd.h>

typedef struct MVar MVar;

MVar *mvar_new(size_t byteSize, int max_wait_ms);
MVar *mvar_lookup(const char *name);
void mvar_destroy(MVar *mvar);

int mvar_take(MVar *mvar, void *localDataPtr, StgStablePtr tso);
int mvar_trytake(MVar *mvar, void *localDataPtr);
int mvar_put(MVar *mvar, void *localDataPtr, StgStablePtr tso);
int mvar_tryput(MVar *mvar, void *localDataPtr);
int mvar_read(MVar *mvar, void *localDataPtr, StgStablePtr tso);
int mvar_tryread(MVar *mvar, void *localDataPtr);
int mvar_swap(MVar *mvar, void *inPtr, void *outPtr, StgStablePtr tso);
int mvar_tryswap(MVar *mvar, void *inPtr, void *outPtr);
int mvar_isempty(MVar *mvar);

typedef struct MVarState {
  pthread_mutex_t mvMut;
  pthread_cond_t canPutC;
  pthread_cond_t canTakeC;
  size_t dataSize;
  pthread_mutexattr_t mvMAttr;
  pthread_condattr_t condAttr;
  int isFull;
  int pendingReaders;
  int totalUsers;
  /**
   * Maximum wait time in milliseconds.
   * This determines maximum possible delay the mvar operation cancels in
   * the event of an async exception thrown to the calling Haskell thread.
   */
  int maxWaitMs;
} MVarState;

typedef struct MVar {
  /**
   * State is stored in the shared data area, accessed by all threads.
   * It has a fixed size and kept at the beginning of a shared memory region.
   */
  MVarState *statePtr;
  /** Actual data is stored next to the MVarState */
  void *dataPtr;
  /** Name of the shared memory region, used to share mvar across processes. */
  SharedObjectName mvarName;
} MVar;

// ensure 64 byte alignment (maximum possible we can think of, e.g. AVX512)
size_t mvar_state_size64() {
  size_t x = sizeof(MVarState);
  return (x + 63) & (~63);
}

MVar *mvar_new(size_t byteSize, int max_wait_ms) {
  int r = 0;
  // allocate MVar
  MVar *mvar = malloc(sizeof(MVar));
  if (mvar == NULL) {
    return NULL;
  }
  genSharedObjectName(mvar->mvarName);

  // allocate shared memory for MVarState and data
  size_t dataShift = mvar_state_size64();
  size_t totalSize = dataShift + byteSize;
  int memFd = shm_open(mvar->mvarName, O_CREAT | O_RDWR, S_IRUSR | S_IWUSR);
  if (memFd < 0) {
    free(mvar);
    return NULL;
  }
  r = ftruncate(memFd, totalSize);
  if (r != 0) {
    shm_unlink(mvar->mvarName);
    free(mvar);
    return NULL;
  }
  mvar->statePtr = (MVarState *)mmap(NULL, totalSize, PROT_READ | PROT_WRITE,
                                     MAP_SHARED, memFd, 0);
  if (mvar->statePtr == MAP_FAILED) {
    shm_unlink(mvar->mvarName);
    free(mvar);
    return NULL;
  }
  mvar->dataPtr = ((void *)(mvar->statePtr)) + dataShift;

  // setup state
  *(mvar->statePtr) = (struct MVarState){.isFull = 0,
                                         .pendingReaders = 0,
                                         .dataSize = byteSize,
                                         .totalUsers = 1,
                                         .maxWaitMs = max_wait_ms};

  // init mutex and condition variables
  r = pthread_mutexattr_init(&(mvar->statePtr->mvMAttr));
#ifdef INTERPROCESS_DEBUG
  if (r == 0)
    r = pthread_mutexattr_settype(&(mvar->statePtr->mvMAttr),
                                  PTHREAD_MUTEX_ERRORCHECK);
#endif
  if (r == 0)
    r = pthread_mutexattr_setpshared(&(mvar->statePtr->mvMAttr),
                                     PTHREAD_PROCESS_SHARED);
  if (r == 0)
    r = pthread_mutex_init(&(mvar->statePtr->mvMut),
                           &(mvar->statePtr->mvMAttr));
  if (r == 0) r = pthread_condattr_init(&(mvar->statePtr->condAttr));
  if (r == 0)
    r = pthread_condattr_setpshared(&(mvar->statePtr->condAttr),
                                    PTHREAD_PROCESS_SHARED);
  if (r == 0)
    r = pthread_cond_init(&(mvar->statePtr->canPutC),
                          &(mvar->statePtr->condAttr));
  if (r == 0)
    r = pthread_cond_init(&(mvar->statePtr->canTakeC),
                          &(mvar->statePtr->condAttr));
  if (r != 0) {
    munmap(mvar->statePtr, totalSize);
    shm_unlink(mvar->mvarName);
    free(mvar);
    return NULL;
  }

  return mvar;
}

MVar *mvar_lookup(const char *name) {
  int memFd = shm_open(name, O_RDWR, S_IRUSR | S_IWUSR);
  if (memFd < 0) return NULL;

  // first, map only sizeof(MVarState) bytes
  // then, read the actual size and remap memory
  void *mvs0 = mmap(NULL, sizeof(MVarState), PROT_READ, MAP_SHARED, memFd, 0);
  if (mvs0 == MAP_FAILED) return NULL;
  size_t dataShift = mvar_state_size64(),
         storeSize = dataShift + ((MVarState *)mvs0)->dataSize;
  munmap(mvs0, sizeof(MVarState));  // don't really care if it is failed
  MVarState *mvs = (MVarState *)mmap(NULL, storeSize, PROT_READ | PROT_WRITE,
                                     MAP_SHARED, memFd, 0);
  if (mvs == MAP_FAILED) return NULL;
  // setup MVar struct
  MVar *mvar = malloc(sizeof(MVar));
  if (mvar == NULL) {
    munmap(mvs, storeSize);
    return NULL;
  }
  mvar->statePtr = (MVarState *)mvs;
  mvar->dataPtr = ((void *)(mvar->statePtr)) + dataShift;
  strcpy(mvar->mvarName, name);

  // update state
  int r = 0;
  r = pthread_mutex_lock(&(mvar->statePtr->mvMut));
  if (r != 0 || mvar->statePtr->totalUsers == 0) {
    munmap(mvar->statePtr, storeSize);
    free(mvar);
    return NULL;
  }
  mvar->statePtr->totalUsers++;
  r = pthread_mutex_unlock(&(mvar->statePtr->mvMut));
  if (r != 0) {
    munmap(mvar->statePtr, storeSize);
    free(mvar);
    return NULL;
  }

  return mvar;
}

void mvar_destroy(MVar *mvar) {
  int usersLeft;
  size_t storeSize = mvar->statePtr->dataSize + mvar_state_size64();
  pthread_mutex_lock(&(mvar->statePtr->mvMut));
  usersLeft = --(mvar->statePtr->totalUsers);
  pthread_mutex_unlock(&(mvar->statePtr->mvMut));
  if (usersLeft > 0) {
    INTERPROCESS_LOG_DEBUG(
        "Destroying local instance of mvar %s, %d users left.\n",
        mvar->mvarName, usersLeft);
    munmap(mvar->statePtr, storeSize);
  } else {
    INTERPROCESS_LOG_DEBUG(
        "Destroying mvar %s globally (no other users left).\n", mvar->mvarName);
    pthread_cond_destroy(&(mvar->statePtr->canTakeC));
    pthread_cond_destroy(&(mvar->statePtr->canPutC));
    pthread_condattr_destroy(&(mvar->statePtr->condAttr));
    pthread_mutex_destroy(&(mvar->statePtr->mvMut));
    pthread_mutexattr_destroy(&(mvar->statePtr->mvMAttr));
    munmap(mvar->statePtr, storeSize);
    shm_unlink(mvar->mvarName);
  }
  free(mvar);
}

static inline void finilize_USE_MUTEX(pthread_mutex_t **mp) {
  pthread_mutex_unlock(*mp);
}

#define USE_MUTEX(state)                                               \
  {                                                                    \
    int r = 0;                                                         \
    r = pthread_mutex_lock(&(state->mvMut));                           \
    if (r != 0) return r;                                              \
  }                                                                    \
  __attribute__((cleanup(finilize_USE_MUTEX))) pthread_mutex_t *_mut = \
      &(state->mvMut);

#define WAIT_A_BIT(cond, mut, ms)                             \
  {                                                           \
    struct timespec time_to_wait;                             \
    clock_gettime(CLOCK_REALTIME, &time_to_wait);             \
    div_t t = div(ms, 1000);                                  \
    time_to_wait.tv_sec += t.quot;                            \
    time_to_wait.tv_nsec += ((long)(t.rem)) * 1000000;        \
    int r = pthread_cond_timedwait(cond, mut, &time_to_wait); \
    if (r != 0 && r != ETIMEDOUT) return r;                   \
  }

int mvar_take(MVar *mvar, void *localDataPtr, StgStablePtr tso) {
  MVarState *state = mvar->statePtr;
  USE_MUTEX(state);
  while (!(state->isFull) || state->pendingReaders > 0) {
    if (state->pendingReaders > 0) pthread_cond_broadcast(&(state->canTakeC));
    WAIT_A_BIT(&(state->canTakeC), &(state->mvMut), state->maxWaitMs);
    if (has_blocked_exceptions(tso)) return EINTR;
  }
  memcpy(localDataPtr, mvar->dataPtr, state->dataSize);
  state->isFull = 0;
  pthread_cond_signal(&(state->canPutC));
  return 0;
}

int mvar_trytake(MVar *mvar, void *localDataPtr) {
  MVarState *state = mvar->statePtr;
  USE_MUTEX(state);
  // shortcut if is empty
  if (!(state->isFull)) return 1;
  while (state->pendingReaders > 0) {
    // make sure readers do not sleep
    pthread_cond_broadcast(&(state->canTakeC));
    // last reader should wake me up, wait for it.
    WAIT_A_BIT(&(state->canTakeC), &(state->mvMut), state->maxWaitMs);
    // repeat emptyness check (if another trytake was faster)
    if (!(state->isFull)) {
      return 1;
    }
  }
  memcpy(localDataPtr, mvar->dataPtr, state->dataSize);
  state->isFull = 0;
  pthread_cond_signal(&(state->canPutC));
  return 0;
}

int mvar_put(MVar *mvar, void *localDataPtr, StgStablePtr tso) {
  MVarState *state = mvar->statePtr;
  USE_MUTEX(state);
  while (state->isFull) {
    WAIT_A_BIT(&(state->canPutC), &(state->mvMut), state->maxWaitMs);
    if (has_blocked_exceptions(tso)) return EINTR;
  }
  memcpy(mvar->dataPtr, localDataPtr, state->dataSize);
  state->isFull = 1;
  pthread_cond_broadcast(&(state->canTakeC));
  return 0;
}

int mvar_tryput(MVar *mvar, void *localDataPtr) {
  MVarState *state = mvar->statePtr;
  USE_MUTEX(state);
  // shortcut if is full
  if (state->isFull) return 1;
  memcpy(mvar->dataPtr, localDataPtr, state->dataSize);
  state->isFull = 1;
  pthread_cond_broadcast(&(state->canTakeC));
  return 0;
}

// Split mvar_read/worker to make sure pendingReaders counter is never leaked
static inline int mvar_read_loop(MVarState *state, StgStablePtr tso) {
  while (!(state->isFull)) {
    WAIT_A_BIT(&(state->canTakeC), &(state->mvMut), state->maxWaitMs);
    if (has_blocked_exceptions(tso)) return EINTR;
  }
  return 0;
}

int mvar_read(MVar *mvar, void *localDataPtr, StgStablePtr tso) {
  MVarState *state = mvar->statePtr;
  USE_MUTEX(state);
  state->pendingReaders++;
  int r = mvar_read_loop(state, tso);
  if (r == 0) memcpy(localDataPtr, mvar->dataPtr, state->dataSize);
  if ((--(state->pendingReaders)) == 0) pthread_cond_signal(&(state->canTakeC));
  return r;
}

int mvar_tryread(MVar *mvar, void *localDataPtr) {
  MVarState *state = mvar->statePtr;
  USE_MUTEX(state);
  // shortcut if is empty
  if (!(state->isFull)) return 1;
  memcpy(localDataPtr, mvar->dataPtr, state->dataSize);
  return 0;
}

int mvar_swap(MVar *mvar, void *inPtr, void *outPtr, StgStablePtr tso) {
  MVarState *state = mvar->statePtr;
  USE_MUTEX(state);
  while (!(state->isFull) || state->pendingReaders > 0) {
    if (state->pendingReaders > 0) {
      pthread_cond_broadcast(&(state->canTakeC));
    }
    WAIT_A_BIT(&(state->canTakeC), &(state->mvMut), state->maxWaitMs);
    if (has_blocked_exceptions(tso)) return EINTR;
  }
  memcpy(outPtr, mvar->dataPtr, state->dataSize);
  memcpy(mvar->dataPtr, inPtr, state->dataSize);
  pthread_cond_signal(&(state->canTakeC));
  return 0;
}

int mvar_tryswap(MVar *mvar, void *inPtr, void *outPtr) {
  MVarState *state = mvar->statePtr;
  USE_MUTEX(state);
  // shortcut if is empty
  if (!(state->isFull)) return 1;
  while (state->pendingReaders > 0) {
    // make sure readers do not sleep
    pthread_cond_broadcast(&(state->canTakeC));
    // last reader should wake me up, wait for it.
    WAIT_A_BIT(&(state->canTakeC), &(state->mvMut), state->maxWaitMs);
    // repeat emptyness check (if another trytake was faster)
    if (!(state->isFull)) return 1;
  }
  memcpy(outPtr, mvar->dataPtr, state->dataSize);
  memcpy(mvar->dataPtr, inPtr, state->dataSize);
  pthread_cond_signal(&(state->canPutC));
  return 0;
}

int mvar_isempty(MVar *mvar) { return mvar->statePtr->isFull == 0; }

void mvar_name(MVar *mvar, char *const name) { strcpy(name, mvar->mvarName); }
