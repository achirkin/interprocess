#include <common.h>

#include <assert.h>
#include <errno.h>
#include <limits.h>
#include <stdatomic.h>
#include <stdlib.h>
#include <windows.h>

#ifndef WAIT_OBJECT_1
#define WAIT_OBJECT_1 ((WAIT_OBJECT_0) + 1)
#endif

#ifndef WAIT_ABANDONED_1
#define WAIT_ABANDONED_1 ((WAIT_ABANDONED_0) + 1)
#endif

#ifdef INTERPROCESS_DEBUG
#define INTERPROCESS_CHECK(op) assert(op)
#else
#define INTERPROCESS_CHECK(op) op
#endif

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
  atomic_int pendingReaders;
} MVarState;

typedef struct MVar {
  /** Manual-reset event: takers and readers wait on this. */
  HANDLE canTakeE;
  /**
   * Mutex: guard the critical region executed by multiple processes.
   * It's placed between the events to allow easy passing of the
   * mutex+event pair to the WaitForMultipleObjects function.
   */
  HANDLE guardM;
  /** Manual-reset event: putters wait on this. */
  HANDLE canPutE;
  /** FileMapping: keep the data by this handle. */
  HANDLE dataStoreFM;
  /**
   * State is stored in the shared data area, accessed by all processes.
   * It has a fixed size and kept at the beginning of a shared memory region.
   */
  MVarState *statePtr;
  /** Actual data is stored next to the MVarState */
  void *dataPtr;
  /**
   * Secondary objects are contstructed by appending a single char to the name.
   */
  SharedObjectName mvarName;
} MVar;

/** Get the pending readers counter value. */
static inline int get_pendingReaders(MVar *mvar) {
  return atomic_load_explicit(&(mvar->statePtr->pendingReaders),
                              memory_order_relaxed);
}

/** Set the pending readers counter value to zero. */
static inline int reset_pendingReaders(MVar *mvar) {
  atomic_store_explicit(&(mvar->statePtr->pendingReaders), 0,
                        memory_order_relaxed);
}

/** Atomically increment the pending readers counter. */
static inline void inc_pendingReaders(MVar *mvar) {
  atomic_fetch_add_explicit(&(mvar->statePtr->pendingReaders), 1,
                            memory_order_relaxed);
}

/**
 * Atomically decrement the pending readers counter, protected against
 * decrementing below zero.
 */
static inline void dec_pendingReaders(MVar *mvar) {
  atomic_int *p = &(mvar->statePtr->pendingReaders);
  int exp = atomic_load_explicit(p, memory_order_relaxed);
  while (exp >= 1) {
    if (atomic_compare_exchange_weak_explicit(
            p, &exp, exp - 1, memory_order_relaxed, memory_order_relaxed))
      return;
  }
}

/** Check if an event is in the signalled state. */
static inline bool is_signalled(HANDLE event) {
  DWORD r = WaitForSingleObject(event, 0);
#ifdef INTERPROCESS_DEBUG
  if (r != WAIT_TIMEOUT && r != WAIT_OBJECT_0)
    INTERPROCESS_LOG_DEBUG(
        "Warning: WaitForSingleObject produced unexpected return code %d;  "
        "error code %d. Continue as if the object is not signalled...\n",
        r, GetLastError());
#endif
  return r == WAIT_OBJECT_0;
}

/**
 * @brief Check the result of waiting and possibly release the mutex.
 *
 * @param [in] debug_name provide a text name for debug logging
 * @param [in] mvar MVar
 * @param [in] code wait return code
 * @param [in] primaryE primary event (canTakeE, canPutE, or NULL as default -
 * signalled).
 * @param [in] secondaryE secondary event (canPutE, canTakeR, or NULL as default
 * - non-signalled).
 * @return true The mutex is owned; the wait is successful and the primary event
 * is signalled.
 * @return false The mutex is not owned; the wait failed or the primary event is
 * not signalled.
 */
static inline bool interpret_wait(const char *debug_name, MVar *mvar,
                                  DWORD code, HANDLE primaryE,
                                  HANDLE secondaryE) {
  bool primaryR = true, secondaryR = false;
  switch (code) {
    // success, but another process is crashed
    case WAIT_ABANDONED_0:
    case WAIT_ABANDONED_1:
      secondaryR = secondaryE != NULL && is_signalled(secondaryE);

    // success
    case WAIT_OBJECT_0:
    case WAIT_OBJECT_1:
      primaryR = primaryE == NULL || is_signalled(primaryE);
      if (secondaryR && primaryR) {
        /*
          If a process crashes while executing one of the MVar operations,
          the state of MVar may become corrupted. This may be the case if the
          mutex is in the abandoned state.
          To recover the state of the MVar, we look at the two events, which
          are carefully set and reset in an order that guarantees the following:

          canTakeE  canPutE   data     state
             +         +      valid   incorrect
             +         -      valid    correct
             -         +        ?      correct
             -         -        ?     impossible

          Also the `pendingReaders` counter may be invalid, but the `take`
          functions detect and reset that.

          The enclosing `if` block can be entered only when the mutex is
          abandoned by another thread, and we can reset either of the two
          events, because the data inside is valid. We choose to reset the
          secondary event to reduce the waiting time and the chance of
          deadlocks.
         */
        INTERPROCESS_CHECK(ResetEvent(secondaryE));
      }
      if (!primaryR) {
        INTERPROCESS_CHECK(ReleaseMutex(mvar->guardM));
      }
      return primaryR;

    // proper failure
    case WAIT_FAILED:
      INTERPROCESS_LOG_DEBUG("Error in %s: wait failed; error code %d.\n",
                             debug_name, GetLastError());
      return false;

    // bug-like failure
    default:
      INTERPROCESS_LOG_DEBUG(
          "Error in %s: unexpected return code %d; error code %d.\n",
          debug_name, code, GetLastError());
      ReleaseMutex(mvar->guardM);
      return false;
  }
}

// ensure 64 byte alignment (maximum possible we can think of, e.g. AVX512)
static inline size_t mvar_state_size64() {
  size_t x = sizeof(MVarState);
  return (x + 63) & (~63);
}

MVar *mvar_new(size_t byteSize, int max_wait_ms) {
  // allocate MVar
  MVar *mvar = malloc(sizeof(*mvar));
  if (mvar == NULL) goto failed;
  genSharedObjectName(mvar->mvarName);

  size_t dataShift = mvar_state_size64();
  size_t totalSize = dataShift + byteSize;
  mvar->dataStoreFM = CreateFileMappingA(
      /* use paging file */
      INVALID_HANDLE_VALUE,
      /* default security */
      NULL,
      /* read/write access */
      PAGE_READWRITE,
      /* maximum object size (high-order DWORD) */
      (DWORD)(totalSize >> 32),
      /* maximum object size (low-order DWORD) */
      (DWORD)(totalSize & 0xFFFFFFFF),
      /* name of mapping object */
      mvar->mvarName);

  if (mvar->dataStoreFM == NULL) {
    INTERPROCESS_LOG_DEBUG("Could not create file mapping object (%d).\n",
                           GetLastError());
    goto failed_after_mvar_created;
  }
  mvar->statePtr = (MVarState *)MapViewOfFile(
      /* handle to map object */
      mvar->dataStoreFM,
      /* read/write permission */
      FILE_MAP_ALL_ACCESS, 0, 0, totalSize);

  if (mvar->statePtr == NULL) {
    INTERPROCESS_LOG_DEBUG("Could not map view of file (%d).\n",
                           GetLastError());
    goto failed_after_fm_created;
  }
  mvar->dataPtr = ((void *)(mvar->statePtr)) + dataShift;

  // create all synchronization objects
  SharedObjectName objName = {0};
  strcpy(objName, mvar->mvarName);
  strcat(objName, "T");
  mvar->canTakeE = CreateEventA(NULL, TRUE, FALSE, objName);
  if (mvar->canTakeE == NULL) {
    INTERPROCESS_LOG_DEBUG("Could not create canTakeE (%d).\n", GetLastError());
    goto failed_after_fm_mapped;
  }
  strcpy(objName, mvar->mvarName);
  strcat(objName, "P");
  mvar->canPutE = CreateEventA(NULL, TRUE, TRUE, objName);
  if (mvar->canPutE == NULL) {
    INTERPROCESS_LOG_DEBUG("Could not create canPutE (%d).\n", GetLastError());
    goto failed_after_cantake;
  }
  strcpy(objName, mvar->mvarName);
  strcat(objName, "M");
  mvar->guardM = CreateMutexA(NULL, FALSE, objName);
  if (mvar->guardM == NULL) {
    INTERPROCESS_LOG_DEBUG("Could not create guardM (%d).\n", GetLastError());
    goto failed_after_canput;
  }

  // zero readers pending
  *(mvar->statePtr) = (struct MVarState){
      .dataSize = byteSize, .maxWaitMs = max_wait_ms, .pendingReaders = 0};

  return mvar;

failed_after_canput:
  CloseHandle(mvar->canPutE);
failed_after_cantake:
  CloseHandle(mvar->canTakeE);
failed_after_fm_mapped:
  UnmapViewOfFile(mvar->statePtr);
failed_after_fm_created:
  CloseHandle(mvar->dataStoreFM);
failed_after_mvar_created:
  free(mvar);
failed:
  return NULL;
}

MVar *mvar_lookup(const char *name) {
  // allocate MVar
  MVar *mvar = malloc(sizeof(*mvar));
  if (mvar == NULL) goto failed;
  strcpy(mvar->mvarName, name);

  mvar->dataStoreFM =
      OpenFileMappingA(FILE_MAP_ALL_ACCESS, FALSE, mvar->mvarName);

  if (mvar->dataStoreFM == NULL) {
    INTERPROCESS_LOG_DEBUG("Could not open file mapping object (%d).\n",
                           GetLastError());
    goto failed_after_mvar_created;
  }

  size_t dataShift = mvar_state_size64();
  mvar->statePtr = (MVarState *)MapViewOfFile(
      /* handle to map object */
      mvar->dataStoreFM,
      /* First time just read the header */
      FILE_MAP_READ, 0, 0, dataShift);

  if (mvar->statePtr == NULL) {
    INTERPROCESS_LOG_DEBUG("Could not map view of file (%d).\n",
                           GetLastError());
    goto failed_after_fm_created;
  }
  size_t totalSize = mvar->statePtr->dataSize + dataShift;
  if (UnmapViewOfFile(mvar->statePtr) == 0) {
    INTERPROCESS_LOG_DEBUG("Could not unmap view of file during lookup (%d).\n",
                           GetLastError());
  }

  mvar->statePtr = (MVarState *)MapViewOfFile(
      /* handle to map object */
      mvar->dataStoreFM,
      /* read/write permission */
      FILE_MAP_ALL_ACCESS, 0, 0, totalSize);

  if (mvar->statePtr == NULL) {
    INTERPROCESS_LOG_DEBUG("Could not map view of file (%d).\n",
                           GetLastError());
    goto failed_after_fm_created;
  }
  mvar->dataPtr = (void *)(mvar->statePtr) + dataShift;

  // lookup all synchronization objects
  SharedObjectName objName = {0};
  strcpy(objName, mvar->mvarName);
  strcat(objName, "T");
  mvar->canTakeE = OpenEventA(EVENT_MODIFY_STATE | SYNCHRONIZE, FALSE, objName);
  if (mvar->canTakeE == NULL) {
    INTERPROCESS_LOG_DEBUG("Could not open canTakeE (%d).\n", GetLastError());
    goto failed_after_fm_mapped;
  }
  strcpy(objName, mvar->mvarName);
  strcat(objName, "P");
  mvar->canPutE = OpenEventA(EVENT_MODIFY_STATE | SYNCHRONIZE, FALSE, objName);
  if (mvar->canPutE == NULL) {
    INTERPROCESS_LOG_DEBUG("Could not open canPutE (%d).\n", GetLastError());
    goto failed_after_cantake;
  }
  strcpy(objName, mvar->mvarName);
  strcat(objName, "M");
  mvar->guardM = OpenMutexA(SYNCHRONIZE, FALSE, objName);
  if (mvar->guardM == NULL) {
    INTERPROCESS_LOG_DEBUG("Could not open guardM (%d).\n", GetLastError());
    goto failed_after_canput;
  }

  return mvar;

failed_after_canput:
  CloseHandle(mvar->canPutE);
failed_after_cantake:
  CloseHandle(mvar->canTakeE);
failed_after_fm_mapped:
  UnmapViewOfFile(mvar->statePtr);
failed_after_fm_created:
  CloseHandle(mvar->dataStoreFM);
failed_after_mvar_created:
  free(mvar);
failed:
  return NULL;
}

void mvar_destroy(MVar *mvar) {
  CloseHandle(mvar->canTakeE);
  CloseHandle(mvar->canPutE);
  CloseHandle(mvar->guardM);
  UnmapViewOfFile(mvar->statePtr);
  CloseHandle(mvar->dataStoreFM);
  free(mvar);
}

int mvar_take(MVar *mvar, void *localDataPtr, StgStablePtr tso) {
  DWORD r;
  int pr_prev = INT_MAX, pr_cur;
another_attempt:
  do {
    r = WaitForMultipleObjects(2, &(mvar->canTakeE), TRUE,
                               mvar->statePtr->maxWaitMs);
    if (r == WAIT_TIMEOUT && has_blocked_exceptions(tso)) return EINTR;
  } while (r == WAIT_TIMEOUT);

  if (!interpret_wait("mvar_take", mvar, r, NULL, mvar->canPutE)) return 1;

  pr_cur = get_pendingReaders(mvar);
  if (pr_cur >= pr_prev) {
    pr_cur == 0;
    reset_pendingReaders(mvar);
  }
  pr_prev = pr_cur;
  if (pr_cur == 0) {
    memcpy(localDataPtr, mvar->dataPtr, mvar->statePtr->dataSize);
    INTERPROCESS_CHECK(SetEvent(mvar->canPutE));
    INTERPROCESS_CHECK(ResetEvent(mvar->canTakeE));
    INTERPROCESS_CHECK(ReleaseMutex(mvar->guardM));
    return 0;
  } else {
    INTERPROCESS_CHECK(ReleaseMutex(mvar->guardM));
    goto another_attempt;
  }
}

int mvar_trytake(MVar *mvar, void *localDataPtr) {
  int pr_prev = INT_MAX, pr_cur;
another_attempt:
  if (!interpret_wait("mvar_trytake", mvar,
                      WaitForSingleObject(mvar->guardM, INFINITE),
                      mvar->canTakeE, mvar->canPutE))
    return 1;

  pr_cur = get_pendingReaders(mvar);
  if (pr_cur >= pr_prev) {
    pr_cur == 0;
    reset_pendingReaders(mvar);
  }
  pr_prev = pr_cur;
  if (pr_cur == 0) {
    memcpy(localDataPtr, mvar->dataPtr, mvar->statePtr->dataSize);
    INTERPROCESS_CHECK(SetEvent(mvar->canPutE));
    INTERPROCESS_CHECK(ResetEvent(mvar->canTakeE));
    INTERPROCESS_CHECK(ReleaseMutex(mvar->guardM));
    return 0;
  } else {
    INTERPROCESS_CHECK(ReleaseMutex(mvar->guardM));
    goto another_attempt;
  }
}

int mvar_put(MVar *mvar, void *localDataPtr, StgStablePtr tso) {
  DWORD r;
  do {
    r = WaitForMultipleObjects(2, &(mvar->guardM), TRUE,
                               mvar->statePtr->maxWaitMs);
    if (r == WAIT_TIMEOUT && has_blocked_exceptions(tso)) return EINTR;
  } while (r == WAIT_TIMEOUT);

  if (!interpret_wait("mvar_put", mvar, r, NULL, mvar->canTakeE)) return 1;

  memcpy(mvar->dataPtr, localDataPtr, mvar->statePtr->dataSize);
  INTERPROCESS_CHECK(SetEvent(mvar->canTakeE));
  INTERPROCESS_CHECK(ResetEvent(mvar->canPutE));
  INTERPROCESS_CHECK(ReleaseMutex(mvar->guardM));
  return 0;
}

int mvar_tryput(MVar *mvar, void *localDataPtr) {
  if (!interpret_wait("mvar_tryput", mvar,
                      WaitForSingleObject(mvar->guardM, INFINITE),
                      mvar->canPutE, mvar->canTakeE))
    return 1;

  memcpy(mvar->dataPtr, localDataPtr, mvar->statePtr->dataSize);
  INTERPROCESS_CHECK(SetEvent(mvar->canTakeE));
  INTERPROCESS_CHECK(ResetEvent(mvar->canPutE));
  INTERPROCESS_CHECK(ReleaseMutex(mvar->guardM));
  return 0;
}

int mvar_read(MVar *mvar, void *localDataPtr, StgStablePtr tso) {
  DWORD r;
  r = WaitForSingleObject(mvar->guardM, INFINITE);
  if (r == WAIT_OBJECT_0 || r == WAIT_ABANDONED_0) {
    if (!is_signalled(mvar->canTakeE)) {
      inc_pendingReaders(mvar);
      INTERPROCESS_CHECK(ReleaseMutex(mvar->guardM));
      do {
        r = WaitForMultipleObjects(2, &(mvar->canTakeE), TRUE,
                                   mvar->statePtr->maxWaitMs);
        if (r == WAIT_TIMEOUT && has_blocked_exceptions(tso)) {
          dec_pendingReaders(mvar);
          return EINTR;
        }
      } while (r == WAIT_TIMEOUT);
      dec_pendingReaders(mvar);
    }
  }
  if (!interpret_wait("mvar_read", mvar, r, NULL, mvar->canPutE)) return 1;

  memcpy(localDataPtr, mvar->dataPtr, mvar->statePtr->dataSize);
  INTERPROCESS_CHECK(ReleaseMutex(mvar->guardM));
  return 0;
}

int mvar_tryread(MVar *mvar, void *localDataPtr) {
  if (!interpret_wait("mvar_tryread", mvar,
                      WaitForSingleObject(mvar->guardM, INFINITE),
                      mvar->canTakeE, mvar->canPutE))
    return 1;

  memcpy(localDataPtr, mvar->dataPtr, mvar->statePtr->dataSize);
  INTERPROCESS_CHECK(ReleaseMutex(mvar->guardM));
  return 0;
}

int mvar_swap(MVar *mvar, void *inPtr, void *outPtr, StgStablePtr tso) {
  DWORD r;
  do {
    r = WaitForMultipleObjects(2, &(mvar->canTakeE), TRUE,
                               mvar->statePtr->maxWaitMs);
    if (r == WAIT_TIMEOUT && has_blocked_exceptions(tso)) return EINTR;
  } while (r == WAIT_TIMEOUT);

  if (!interpret_wait("mvar_swap", mvar, r, NULL, mvar->canPutE)) return 1;

  memcpy(outPtr, mvar->dataPtr, mvar->statePtr->dataSize);
  INTERPROCESS_CHECK(SetEvent(mvar->canPutE));
  INTERPROCESS_CHECK(ResetEvent(mvar->canTakeE));
  memcpy(mvar->dataPtr, inPtr, mvar->statePtr->dataSize);
  INTERPROCESS_CHECK(SetEvent(mvar->canTakeE));
  INTERPROCESS_CHECK(ResetEvent(mvar->canPutE));
  INTERPROCESS_CHECK(ReleaseMutex(mvar->guardM));
  return 0;
}

int mvar_tryswap(MVar *mvar, void *inPtr, void *outPtr) {
  if (!interpret_wait("mvar_tryswap", mvar,
                      WaitForSingleObject(mvar->guardM, INFINITE),
                      mvar->canTakeE, mvar->canPutE))
    return 1;

  memcpy(outPtr, mvar->dataPtr, mvar->statePtr->dataSize);
  INTERPROCESS_CHECK(SetEvent(mvar->canPutE));
  INTERPROCESS_CHECK(ResetEvent(mvar->canTakeE));
  memcpy(mvar->dataPtr, inPtr, mvar->statePtr->dataSize);
  INTERPROCESS_CHECK(SetEvent(mvar->canTakeE));
  INTERPROCESS_CHECK(ResetEvent(mvar->canPutE));
  INTERPROCESS_CHECK(ReleaseMutex(mvar->guardM));
  return 0;
}

int mvar_isempty(MVar *mvar) { return is_signalled(mvar->canTakeE) ? 0 : 1; }

void mvar_name(MVar *mvar, char *const name) { strcpy(name, mvar->mvarName); }
