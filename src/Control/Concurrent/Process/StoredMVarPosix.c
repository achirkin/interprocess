#include <common.h>

#include <assert.h>
#include <errno.h>
#include <fcntl.h>
#include <pthread.h>
#include <stdatomic.h>
#include <string.h>
#include <sys/mman.h>
#include <time.h>
#include <unistd.h>

#ifdef INTERPROCESS_DEBUG
#define ASSERT_ZERO(op) assert(op == 0)
#else
#define ASSERT_ZERO(op) op
#endif

// NB: use robust mutexes
// https://man7.org/linux/man-pages/man3/pthread_mutexattr_setrobust.3.html

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
  size_t dataSize;
  /**
   * Maximum wait time in milliseconds.
   * This determines maximum possible delay the mvar operation cancels in
   * the event of an async exception thrown to the calling Haskell thread.
   */
  int maxWaitMs;
  int pendingReaders;

  // following is OS-specific

  int isFull;
  atomic_int totalUsers;
  pthread_mutex_t mvMut;
  pthread_cond_t canPutC;
  pthread_cond_t canTakeC;
} MVarState;

typedef struct MVar {
  /**
   * State is stored in the shared data area, accessed by all processes.
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

/**
 * Increase the total users counter by one and return zero;
 * if there are no users left (counter == 0), fail (return non-zero).
 */
static inline int inc_totalUsers(MVar *mvar) {
  atomic_int *p = &(mvar->statePtr->totalUsers);
  int exp = atomic_load_explicit(p, memory_order_relaxed);
  while (exp >= 1) {
    if (atomic_compare_exchange_weak_explicit(
            p, &exp, exp + 1, memory_order_relaxed, memory_order_relaxed))
      return 0;
  }
  return -1;
}

MVar *mvar_new(size_t byteSize, int max_wait_ms) {
  int r = 0;
  // allocate MVar
  MVar *mvar = malloc(sizeof(MVar));
  if (mvar == NULL) goto failed;
  genSharedObjectName(mvar->mvarName);

  // allocate shared memory for MVarState and data
  size_t dataShift = mvar_state_size64();
  size_t totalSize = dataShift + byteSize;
  int memFd = shm_open(mvar->mvarName, O_CREAT | O_RDWR, S_IRUSR | S_IWUSR);
  if (memFd < 0) {
    INTERPROCESS_LOG_DEBUG("Could not open shared memory segment (%d).\n",
                           errno);
    goto failed_after_mvar_allocated;
  }

  if (ftruncate(memFd, totalSize) != 0) {
    INTERPROCESS_LOG_DEBUG(
        "Could not allocate shared memory (ftruncate) (%d).\n", errno);
    goto failed_after_shmem_open;
  }
  mvar->statePtr = (MVarState *)mmap(NULL, totalSize, PROT_READ | PROT_WRITE,
                                     MAP_SHARED, memFd, 0);
  if (mvar->statePtr == MAP_FAILED) {
    INTERPROCESS_LOG_DEBUG("Could not map shared memory (mmap) (%d).\n", errno);
    goto failed_after_shmem_open;
  }
  mvar->dataPtr = ((void *)(mvar->statePtr)) + dataShift;

  // setup state
  *(mvar->statePtr) = (struct MVarState){.isFull = 0,
                                         .pendingReaders = 0,
                                         .dataSize = byteSize,
                                         .totalUsers = 1,
                                         .maxWaitMs = max_wait_ms};

  // init mutex and condition variables
  pthread_mutexattr_t mvMAttr;
  r = pthread_mutexattr_init(&mvMAttr);
  if (r != 0) goto failed_on_mutex_init;
#ifdef INTERPROCESS_DEBUG
  r = pthread_mutexattr_settype(&mvMAttr, PTHREAD_MUTEX_ERRORCHECK);
  if (r != 0) goto failed_on_mutex_init;
#endif
  r = pthread_mutexattr_setpshared(&mvMAttr, PTHREAD_PROCESS_SHARED);
  if (r != 0) goto failed_on_mutex_init;
  r = pthread_mutex_init(&(mvar->statePtr->mvMut), &mvMAttr);
  if (r != 0) goto failed_on_mutex_init;
  r = pthread_mutexattr_destroy(&mvMAttr);
  if (r != 0) {
  failed_on_mutex_init:
    INTERPROCESS_LOG_DEBUG("Could not init a mutex (%d).\n", r);
    goto failed_after_mmap;
  }

  pthread_condattr_t condAttr;
  r = pthread_condattr_init(&condAttr);
  if (r != 0) goto failed_on_cond_init;
  r = pthread_condattr_setpshared(&condAttr, PTHREAD_PROCESS_SHARED);
  if (r != 0) goto failed_on_cond_init;
  r = pthread_cond_init(&(mvar->statePtr->canPutC), &condAttr);
  if (r != 0) goto failed_on_cond_init;
  r = pthread_cond_init(&(mvar->statePtr->canTakeC), &condAttr);
  if (r != 0) goto failed_on_cond_init;
  pthread_condattr_destroy(&condAttr);
  if (r != 0) {
  failed_on_cond_init:
    INTERPROCESS_LOG_DEBUG("Could not init a condition variable (%d).\n", r);
    goto failed_after_mmap;
  }

  return mvar;

failed_after_mmap:
  munmap(mvar->statePtr, totalSize);
failed_after_shmem_open:
  shm_unlink(mvar->mvarName);
failed_after_mvar_allocated:
  free(mvar);
failed:
  return NULL;
}

MVar *mvar_lookup(const char *name) {
  // allocate MVar
  MVar *mvar = malloc(sizeof(MVar));
  if (mvar == NULL) goto failed;
  strcpy(mvar->mvarName, name);

  int memFd = shm_open(name, O_RDWR, S_IRUSR | S_IWUSR);
  if (memFd < 0) {
    INTERPROCESS_LOG_DEBUG("Could not open shared memory segment (%d).\n",
                           errno);
    goto failed_after_mvar_allocated;
  }

  // first, map only sizeof(MVarState) bytes
  // then, read the actual size and remap memory
  mvar->statePtr = (MVarState *)mmap(NULL, sizeof(MVarState), PROT_READ,
                                     MAP_SHARED, memFd, 0);
  if (mvar->statePtr == MAP_FAILED) {
    INTERPROCESS_LOG_DEBUG("Could not map shared memory (mmap) (%d).\n", errno);
    goto failed_after_mvar_allocated;
  }
  size_t dataShift = mvar_state_size64();
  size_t totalSize = dataShift + mvar->statePtr->dataSize;
  ASSERT_ZERO(munmap(mvar->statePtr, sizeof(MVarState)));

  // now we know the actual size, map the memory again
  mvar->statePtr = (MVarState *)mmap(NULL, totalSize, PROT_READ | PROT_WRITE,
                                     MAP_SHARED, memFd, 0);
  if (mvar->statePtr == MAP_FAILED) {
    INTERPROCESS_LOG_DEBUG("Could not map shared memory (mmap) (%d).\n", errno);
    goto failed_after_mvar_allocated;
  }
  mvar->dataPtr = ((void *)(mvar->statePtr)) + dataShift;

  // update state
  if (inc_totalUsers(mvar) != 0) {
    INTERPROCESS_LOG_DEBUG(
        "Found that the mvar was already destroyed when updating the total "
        "users counter.\n");
    goto failed_after_mmap;
  }
  return mvar;

failed_after_mmap:
  munmap(mvar->statePtr, totalSize);
failed_after_mvar_allocated:
  free(mvar);
failed:
  return NULL;
}

void mvar_destroy(MVar *mvar) {
  size_t storeSize = mvar->statePtr->dataSize + mvar_state_size64();
  int usersLeft = atomic_fetch_sub_explicit(&(mvar->statePtr->totalUsers), 1,
                                            memory_order_relaxed) -
                  1;
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
    pthread_mutex_destroy(&(mvar->statePtr->mvMut));
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

int interprocess_cond_timedwait(pthread_cond_t* cond, pthread_mutex_t* mut, int millis) {
    struct timespec time_to_wait;                                        
    clock_gettime(CLOCK_REALTIME, &time_to_wait);                        
    ldiv_t t = ldiv(millis * 1000 + (time_to_wait.tv_nsec / 1000), 1000000); 
    time_to_wait.tv_sec += t.quot;                                       
    time_to_wait.tv_nsec = t.rem * 1000;                                 
    int r = pthread_cond_timedwait(cond, mut, &time_to_wait);
#ifdef __APPLE__
    // Workaround for macOS failing in timedwait for absolutely no reason
    if (r == EINVAL) {
      r = pthread_mutex_unlock(mut);
      if (r != 0) return r;
      t = ldiv(millis, 1000);
      time_to_wait.tv_sec = t.quot;
      time_to_wait.tv_nsec = t.rem * 1000000;
      nanosleep(&time_to_wait, NULL);
      r = pthread_mutex_lock(mut);
    }
#endif
    return r;
}

#define WAIT_A_BIT(cond, mut, ms)                       \
  {                                                     \
    int r = interprocess_cond_timedwait(cond, mut, ms); \
    if (r != 0 && r != ETIMEDOUT) return r;             \
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
