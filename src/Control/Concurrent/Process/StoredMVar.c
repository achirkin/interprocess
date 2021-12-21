#include <Rts.h>
#include <assert.h>
#include <stdlib.h>
#include <string.h>

#include "SharedObjectName.h"
#include "common.h"

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

/**
 * The input is an StgTSO* wrapped in a stable pointer.
 * The StgTSO object can be moved during GC, which can happen while we're
 * in a safe/interruptible foreign call.
 *
 * By performing this check, I can find out if there are any pending async
 * exceptions in the calling (and suspended) Haskell thread. I need this,
 * because on Windows there seem to be no way to interrupt / check the
 * interruption status in between `WaitForSingleObject` or alike using the
 * provided GHC machinery (CancelSynchronousIo).
 */
bool has_blocked_exceptions(StgStablePtr tso) {
  return (typeof(END_TSO_QUEUE))(
             ((StgTSO *)deRefStablePtr(tso))->blocked_exceptions) !=
         END_TSO_QUEUE;
}

#if defined(WIN32) || defined(_WIN32) || defined(__WIN32) || \
    defined(mingw32_HOST_OS)
#include <windows.h>
#ifndef WAIT_OBJECT_1
#define WAIT_OBJECT_1 ((WAIT_OBJECT_0) + 1)
#endif

typedef struct MVarState {
  size_t dataSize;
  int pendingReaders;
  /**
   * Maximum wait time in milliseconds.
   * This determines maximum possible delay the mvar operation cancels in
   * the event of an async exception thrown to the calling Haskell thread.
   */
  int maxWaitMs;
} MVarState;

typedef struct MVar {
  /* Semaphore: readers wait on this.
    mvar_put releases 2n-1 units of semaphore, and every reader takes two units,
    the last reader fails to take a second unit and sets canTakeE.
   */
  HANDLE canReadS;
  /* Auto-reset event: takers wait on this
   */
  HANDLE canTakeE;
  /* Auto-reset event: putters wait on this
   */
  HANDLE canPutE;
  /* Mutex: protect the number of pending readers
   */
  HANDLE protectReaders;
  /* FileMapping: keep the data by this handle
   */
  HANDLE dataStoreH;
  /* Address of the data store
   */
  MVarState *storePtr;
  /* Actual data is stored next to the MVarState
   */
  void *dataPtr;
  /* Base name of the shared memory region, used to share mvar across processes.
     Secondary objects are contstructed by appending a single char to the name.
   */
  SharedObjectName mvarName;
} MVar;

MVar *mvar_new(size_t byteSize, int max_wait_ms) {
  // allocate MVar
  MVar *mvar = malloc(sizeof(MVar));
  if (mvar == NULL) {
    return NULL;
  }
  genSharedObjectName(mvar->mvarName);

  mvar->dataStoreH = CreateFileMappingA(
      INVALID_HANDLE_VALUE  // use paging file
      ,
      NULL  // default security
      ,
      PAGE_READWRITE  // read/write access
      ,
      (DWORD)(byteSize >> 32)  // maximum object size (high-order DWORD)
      ,
      (DWORD)((byteSize + 64) &
              0xFFFFFFFF)  // maximum object size (low-order DWORD)
      ,
      mvar->mvarName);  // name of mapping object

  if (mvar->dataStoreH == NULL) {
    INTERPROCESS_LOG_DEBUG("Could not create file mapping object (%d).\n",
                           GetLastError());
    free(mvar);
    return NULL;
  }
  mvar->storePtr =
      (MVarState *)MapViewOfFile(mvar->dataStoreH  // handle to map object
                                 ,
                                 FILE_MAP_ALL_ACCESS  // read/write permission
                                 ,
                                 0, 0, byteSize + 64);

  if (mvar->storePtr == NULL) {
    INTERPROCESS_LOG_DEBUG("Could not map view of file (%d).\n",
                           GetLastError());
    CloseHandle(mvar->dataStoreH);
    free(mvar);
    return NULL;
  }
  mvar->dataPtr = ((void *)(mvar->storePtr)) + 64;

  // now, create all synchronization objects
  // TODO: NULL value checking?
  SharedObjectName objName = {0};
  strcpy(objName, mvar->mvarName);
  strcat(objName, "T");
  mvar->canTakeE = CreateEventA(NULL, FALSE, FALSE, objName);
  strcpy(objName, mvar->mvarName);
  strcat(objName, "P");
  mvar->canPutE = CreateEventA(NULL, FALSE, TRUE, objName);
  strcpy(objName, mvar->mvarName);
  strcat(objName, "R");
  mvar->canReadS = CreateSemaphoreA(NULL, 0, LONG_MAX, objName);
  strcpy(objName, mvar->mvarName);
  strcat(objName, "M");
  mvar->protectReaders = CreateMutexA(NULL, FALSE, objName);

  // zero readers pending
  *(mvar->storePtr) = (struct MVarState){
      .dataSize = byteSize, .pendingReaders = 0, .maxWaitMs = max_wait_ms};

  return mvar;
}

MVar *mvar_lookup(const char *name) {
  // allocate MVar
  MVar *mvar = malloc(sizeof(MVar));
  if (mvar == NULL) {
    return NULL;
  }
  strcpy(mvar->mvarName, name);

  mvar->dataStoreH =
      OpenFileMappingA(FILE_MAP_ALL_ACCESS, FALSE, mvar->mvarName);

  if (mvar->dataStoreH == NULL) {
    INTERPROCESS_LOG_DEBUG("Could not open file mapping object (%d).\n",
                           GetLastError());
    free(mvar);
    return NULL;
  }
  mvar->storePtr =
      (MVarState *)MapViewOfFile(mvar->dataStoreH  // handle to map object
                                 ,
                                 FILE_MAP_READ  // read/write permission
                                 ,
                                 0, 0, 64);

  if (mvar->storePtr == NULL) {
    INTERPROCESS_LOG_DEBUG("Could not map view of file (%d).\n",
                           GetLastError());
    CloseHandle(mvar->dataStoreH);
    free(mvar);
    return NULL;
  }
  size_t storeSize = mvar->storePtr->dataSize + 64;
  UnmapViewOfFile(mvar->storePtr);
  mvar->storePtr =
      (MVarState *)MapViewOfFile(mvar->dataStoreH  // handle to map object
                                 ,
                                 FILE_MAP_ALL_ACCESS  // read/write permission
                                 ,
                                 0, 0, storeSize);

  if (mvar->storePtr == NULL) {
    INTERPROCESS_LOG_DEBUG("Could not map view of file (%d).\n",
                           GetLastError());
    CloseHandle(mvar->dataStoreH);
    free(mvar);
    return NULL;
  }
  mvar->dataPtr = (void *)(mvar->storePtr) + 64;

  // now, create all synchronization objects
  // TODO: NULL value checking?
  SharedObjectName objName = {0};
  strcpy(objName, mvar->mvarName);
  strcat(objName, "T");
  mvar->canTakeE = OpenEventA(EVENT_MODIFY_STATE | SYNCHRONIZE, FALSE, objName);
  strcpy(objName, mvar->mvarName);
  strcat(objName, "P");
  mvar->canPutE = OpenEventA(EVENT_MODIFY_STATE | SYNCHRONIZE, FALSE, objName);
  strcpy(objName, mvar->mvarName);
  strcat(objName, "R");
  mvar->canReadS =
      OpenSemaphoreA(SEMAPHORE_MODIFY_STATE | SYNCHRONIZE, FALSE, objName);
  strcpy(objName, mvar->mvarName);
  strcat(objName, "M");
  mvar->protectReaders = OpenMutexA(SYNCHRONIZE, FALSE, objName);

  return mvar;
}

void mvar_destroy(MVar *mvar) {
  UnmapViewOfFile(mvar->storePtr);
  CloseHandle(mvar->canTakeE);
  CloseHandle(mvar->canPutE);
  CloseHandle(mvar->canReadS);
  CloseHandle(mvar->protectReaders);
  CloseHandle(mvar->dataStoreH);
  free(mvar);
}

int mvar_take(MVar *mvar, void *localDataPtr, StgStablePtr tso) {
  DWORD r;
  do {
    r = WaitForSingleObject(mvar->canTakeE, mvar->storePtr->maxWaitMs);
    if (r == WAIT_TIMEOUT && has_blocked_exceptions(tso)) return EINTR;
  } while (r == WAIT_TIMEOUT);
  if (r != WAIT_OBJECT_0) {
    INTERPROCESS_LOG_DEBUG(
        "WaitForSingleObject canTakeE error: return %d; error code %d.\n", r,
        GetLastError());
    return 1;
  } else {
    memcpy(localDataPtr, mvar->dataPtr, mvar->storePtr->dataSize);
    SetEvent(mvar->canPutE);
    return 0;
  }
}

int mvar_trytake(MVar *mvar, void *localDataPtr) {
  DWORD r = WaitForSingleObject(mvar->canTakeE, 0);
  switch (r) {
    case WAIT_OBJECT_0:
      memcpy(localDataPtr, mvar->dataPtr, mvar->storePtr->dataSize);
      SetEvent(mvar->canPutE);
      return 0;
    case WAIT_TIMEOUT:
      return 1;
    default:
      INTERPROCESS_LOG_DEBUG(
          "WaitForSingleObject canTakeE error: return %d; error code %d.\n", r,
          GetLastError());
      return 1;
  }
}

int mvar_put(MVar *mvar, void *localDataPtr, StgStablePtr tso) {
  DWORD r;
  do {
    r = WaitForSingleObject(mvar->canPutE, mvar->storePtr->maxWaitMs);
    if (r == WAIT_TIMEOUT && has_blocked_exceptions(tso)) return EINTR;
  } while (r == WAIT_TIMEOUT);
  if (r != WAIT_OBJECT_0) {
    INTERPROCESS_LOG_DEBUG(
        "WaitForSingleObject canPutE error: return %d; error code %d.\n", r,
        GetLastError());
    return 1;
  } else {
    memcpy(mvar->dataPtr, localDataPtr, mvar->storePtr->dataSize);
    // first check readers, and only then, maybe allow takers
    do {
      r = WaitForSingleObject(mvar->protectReaders, mvar->storePtr->maxWaitMs);
      if (r == WAIT_TIMEOUT && has_blocked_exceptions(tso)) return EINTR;
    } while (r == WAIT_TIMEOUT);
    assert(r == WAIT_OBJECT_0);
    int remRdrs = mvar->storePtr->pendingReaders;
    r = ReleaseMutex(mvar->protectReaders);
    assert(r != 0);
    if (remRdrs == 0) {
      SetEvent(mvar->canTakeE);
    } else {
      ReleaseSemaphore(mvar->canReadS, 2 * (LONG)remRdrs - 1, NULL);
    }
    return 0;
  }
}

int mvar_tryput(MVar *mvar, void *localDataPtr) {
  DWORD r = WaitForSingleObject(mvar->canPutE, 0);
  switch (r) {
    case WAIT_OBJECT_0:
      memcpy(mvar->dataPtr, localDataPtr, mvar->storePtr->dataSize);
      // first check readers, and only then, maybe allow takers
      r = WaitForSingleObject(mvar->protectReaders, INFINITE);
      assert(r == WAIT_OBJECT_0);
      int remRdrs = mvar->storePtr->pendingReaders;
      r = ReleaseMutex(mvar->protectReaders);
      assert(r != 0);
      if (remRdrs == 0) {
        SetEvent(mvar->canTakeE);
      } else {
        ReleaseSemaphore(mvar->canReadS, 2 * (LONG)remRdrs - 1, NULL);
      }
      return 0;
    case WAIT_TIMEOUT:
      return 1;
    default:
      INTERPROCESS_LOG_DEBUG(
          "WaitForSingleObject canTakeE error: return %d; error code %d.\n", r,
          GetLastError());
      return 1;
  }
}

int mvar_read(MVar *mvar, void *localDataPtr, StgStablePtr tso) {
  DWORD r;
  do {
    r = WaitForSingleObject(mvar->protectReaders, mvar->storePtr->maxWaitMs);
    if (r == WAIT_TIMEOUT && has_blocked_exceptions(tso)) return EINTR;
  } while (r == WAIT_TIMEOUT);
  assert(r == WAIT_OBJECT_0);
  mvar->storePtr->pendingReaders++;
  r = ReleaseMutex(mvar->protectReaders);
  assert(r != 0);
  DWORD signaled;
  do {
    signaled = WaitForMultipleObjects(2, (HANDLE *)mvar, FALSE,
                                      mvar->storePtr->maxWaitMs);
    if (signaled == WAIT_TIMEOUT && has_blocked_exceptions(tso)) return EINTR;
  } while (signaled == WAIT_TIMEOUT);
  switch (signaled) {
    case WAIT_OBJECT_0:
    case WAIT_OBJECT_1:
      memcpy(localDataPtr, mvar->dataPtr, mvar->storePtr->dataSize);
      do {
        r = WaitForSingleObject(mvar->protectReaders,
                                mvar->storePtr->maxWaitMs);
        if (r == WAIT_TIMEOUT && has_blocked_exceptions(tso)) return EINTR;
      } while (r == WAIT_TIMEOUT);
      assert(r == WAIT_OBJECT_0);
      --(mvar->storePtr->pendingReaders);
      r = ReleaseMutex(mvar->protectReaders);
      assert(r != 0);
      if (signaled == WAIT_OBJECT_0) {
        if (WaitForSingleObject(mvar->canReadS, 0) != WAIT_OBJECT_0) {
          SetEvent(mvar->canTakeE);
        }
      } else if (signaled == WAIT_OBJECT_1) {
        SetEvent(mvar->canTakeE);
      }
      return 0;
    default:
      INTERPROCESS_LOG_DEBUG(
          "(mvar_read) WaitForMultipleObjects error: return %d; error code "
          "%d.\n",
          r, GetLastError());
      return 1;
  }
}

int mvar_tryread(MVar *mvar, void *localDataPtr) {
  DWORD r = WaitForSingleObject(mvar->canTakeE, 0);
  switch (r) {
    case WAIT_OBJECT_0:
      memcpy(localDataPtr, mvar->dataPtr, mvar->storePtr->dataSize);
      SetEvent(mvar->canTakeE);
      return 0;
    case WAIT_TIMEOUT:
      return 1;
    default:
      INTERPROCESS_LOG_DEBUG(
          "WaitForSingleObject canTakeE error: return %d; error code %d.\n", r,
          GetLastError());
      return 1;
  }
}

int mvar_swap(MVar *mvar, void *inPtr, void *outPtr, StgStablePtr tso) {
  DWORD r;
  do {
    r = WaitForSingleObject(mvar->canTakeE, mvar->storePtr->maxWaitMs);
    if (r == WAIT_TIMEOUT && has_blocked_exceptions(tso)) return EINTR;
  } while (r == WAIT_TIMEOUT);
  if (r != WAIT_OBJECT_0) {
    INTERPROCESS_LOG_DEBUG(
        "WaitForSingleObject canTakeE error: return %d; error code %d.\n", r,
        GetLastError());
    return 1;
  } else {
    memcpy(outPtr, mvar->dataPtr, mvar->storePtr->dataSize);
    memcpy(mvar->dataPtr, inPtr, mvar->storePtr->dataSize);
    SetEvent(mvar->canTakeE);
    return 0;
  }
}

int mvar_tryswap(MVar *mvar, void *inPtr, void *outPtr) {
  DWORD r = WaitForSingleObject(mvar->canTakeE, 0);
  switch (r) {
    case WAIT_OBJECT_0:
      memcpy(outPtr, mvar->dataPtr, mvar->storePtr->dataSize);
      memcpy(mvar->dataPtr, inPtr, mvar->storePtr->dataSize);
      SetEvent(mvar->canTakeE);
      return 0;
    case WAIT_TIMEOUT:
      return 1;
    default:
      INTERPROCESS_LOG_DEBUG(
          "WaitForSingleObject canTakeE error: return %d; error code %d.\n", r,
          GetLastError());
      return 1;
  }
}

int mvar_isempty(MVar *mvar) {
  DWORD r = WaitForSingleObject(mvar->canPutE, 0);
  if (r == WAIT_TIMEOUT) {
    return 1;
  } else {
    SetEvent(mvar->canPutE);
    return 0;
  }
}

#else
#include <errno.h>
#include <fcntl.h>
#include <pthread.h>
#include <signal.h>
#include <sys/mman.h>
#include <time.h>
#include <unistd.h>

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

#endif

void mvar_name(MVar *mvar, char *const name) { strcpy(name, mvar->mvarName); }
