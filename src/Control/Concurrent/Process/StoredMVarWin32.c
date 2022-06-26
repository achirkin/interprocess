#include <common.h>

#include <assert.h>
#include <errno.h>
#include <limits.h>
#include <stdatomic.h>
#include <windows.h>

#include <sys\timeb.h> 

#ifndef WAIT_OBJECT_1
#define WAIT_OBJECT_1 ((WAIT_OBJECT_0) + 1)
#endif

#ifndef WAIT_ABANDONED_1
#define WAIT_ABANDONED_1 ((WAIT_ABANDONED_0) + 1)
#endif

#ifdef INTERPROCESS_DEBUG
#define ASSERT_TRUE(op) assert(op)
#else
#define ASSERT_TRUE(op) op
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


struct time_me {
  struct timeb start;
  char* label;
};

static inline struct time_me start_TIME_IT(const char* label, int size) {
  struct time_me t;
  ftime(&(t.start));
  t.label = (char*) malloc (size);
  memcpy(t.label, label, size);
  return t;
}

static inline void finalize_TIME_IT(struct time_me *t) {
  struct timeb stop;
  ftime(&stop);
  int diff = (int)(1000.0 * (stop.time - t->start.time) + (stop.millitm - t->start.millitm));
  INTERPROCESS_LOG_DEBUG("[%s] elapsed time: %d ms\n", t->label, diff);
  free(t->label);
}

#define CAT(a, b) a ## b
#define CAT2(a, b) CAT(a, b)
#define TIME_IT(label) \
  __attribute__((cleanup(finalize_TIME_IT))) struct time_me CAT2(_t, __LINE__) = start_TIME_IT(label, sizeof label);


struct state_me {
  MVar* mvar;
  char* label;
};

static inline struct state_me start_STATE_IT(MVar *m, const char* label, int size) {
  struct state_me s;
  s.mvar = m;
  s.label = (char*) malloc (size);
  memcpy(s.label, label, size);
  INTERPROCESS_LOG_DEBUG("[%s] enter state: take = 0x%x; put = 0x%x\n", s.label, 
    WaitForSingleObject(m->canTakeE, 0), WaitForSingleObject(m->canPutE, 0));
  return s;
}

static inline void finalize_STATE_IT(struct state_me *s) {
  INTERPROCESS_LOG_DEBUG("[%s] exit state: take = 0x%x; put = 0x%x; guard acquired = %d\n", s->label, 
    WaitForSingleObject(s->mvar->canTakeE, 0), WaitForSingleObject(s->mvar->canPutE, 0), ReleaseMutex(s->mvar->guardM) != 0);
  free(s->label);
}

#define STATE_IT(label, mvar) \
  __attribute__((cleanup(finalize_STATE_IT))) struct state_me CAT2(_s, __LINE__) = start_STATE_IT(mvar, label, sizeof label);



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
        ASSERT_TRUE(ResetEvent(secondaryE));
      }
      if (!primaryR) {
        ASSERT_TRUE(ReleaseMutex(mvar->guardM));
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

/** Copy the data from the mvar and switch the events. */
static inline void load_data(MVar *mvar, void *ptr) {
  /*
    The order of set/reset event is important; it ensures that canTakeE and
    canPutE are never both non-signalled to avoid deadlocks. If a process
    crashes between SetEvent and ResetEvent, other processes can detect and fix
    that. See the note in `interpret_wait` for more details.
   */
  memcpy(ptr, mvar->dataPtr, mvar->statePtr->dataSize);
  ASSERT_TRUE(SetEvent(mvar->canPutE));
  ASSERT_TRUE(ResetEvent(mvar->canTakeE));
}

/** Copy the data to the mvar and switch the events. */
static inline void save_data(MVar *mvar, void *ptr) {
  // See the note in `load_data`
  memcpy(mvar->dataPtr, ptr, mvar->statePtr->dataSize);
  ASSERT_TRUE(SetEvent(mvar->canTakeE));
  ASSERT_TRUE(ResetEvent(mvar->canPutE));
}

/**
 * @brief Call `WaitForMultipleObjects` interrupting on exceptions in a calling
 * Haskell thread.
 *
 * This function calls `WaitForMultipleObjects` in a loop with a timeout defined
 * at the mvar creation. After each iteration, it checks if there are any
 * pending blocked exceptions in the calling Haskell thread. If that is the
 * case, it sets the `LastError` to `EINTR` and returns `WAIT_FAILED`. You can
 * view this function as `WaitForMultipleObjects` with the infinite timeout.
 *
 * @param mvar
 * @param tso Stable pointer for the Haskell thread state object (StgTSO).
 * @param handles Two waitable objects, one of which is a mutex.
 * @return DWORD Return code of `WaitForMultipleObjects`.
 */
static inline DWORD wait_interruptible(MVar *mvar, StgStablePtr tso,
                                       HANDLE *handles) {
  DWORD r;
  do {
    {
      INTERPROCESS_LOG_DEBUG("wait_interruptible/WaitForMultipleObjects.Before: %x %x\n",
        WaitForSingleObject(mvar->canTakeE, 0), WaitForSingleObject(mvar->canPutE, 0));
      TIME_IT("wait_interruptible/WaitForMultipleObjects");
      r = WaitForMultipleObjects(2, handles, TRUE, mvar->statePtr->maxWaitMs);
    }
      INTERPROCESS_LOG_DEBUG("wait_interruptible/WaitForMultipleObjects.After: %x %x %x %d\n",
        WaitForSingleObject(mvar->canTakeE, 0), WaitForSingleObject(mvar->canPutE, 0), r, handles == &(mvar->canTakeE));
      // INTERPROCESS_LOG_DEBUG("Error code %d (0x%x)", GetLastError(), GetLastError());
    if (r == WAIT_TIMEOUT) {
      bool hasex;
      {
        TIME_IT("wait_interruptible/has_blocked_exceptions");
        hasex = has_blocked_exceptions(tso);
      }
      if (hasex) {
        SetLastError(EINTR);
        return WAIT_FAILED;
      }
    }
  } while (r == WAIT_TIMEOUT);
  return r;
}

// ensure 64 byte alignment (maximum possible we can think of, e.g. AVX512)
static inline size_t mvar_state_size64() {
  size_t x = sizeof(MVarState);
  return (x + 63) & (~63);
}

MVar *mvar_new(size_t byteSize, int max_wait_ms) {
  TIME_IT("mvar_new");
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
    goto failed_after_mvar_allocated;
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
failed_after_mvar_allocated:
  free(mvar);
failed:
  return NULL;
}

MVar *mvar_lookup(const char *name) {
  TIME_IT("mvar_lookup");
  // allocate MVar
  MVar *mvar = malloc(sizeof(*mvar));
  if (mvar == NULL) goto failed;
  strcpy(mvar->mvarName, name);

  mvar->dataStoreFM =
      OpenFileMappingA(FILE_MAP_ALL_ACCESS, FALSE, mvar->mvarName);

  if (mvar->dataStoreFM == NULL) {
    INTERPROCESS_LOG_DEBUG("Could not open file mapping object (%d).\n",
                           GetLastError());
    goto failed_after_mvar_allocated;
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
failed_after_mvar_allocated:
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
  STATE_IT("mvar_take", mvar);
  TIME_IT("mvar_take");
  int pr_prev = INT_MAX, pr_cur;
  do {
    if (!interpret_wait("mvar_take", mvar,
                        wait_interruptible(mvar, tso, &(mvar->canTakeE)), NULL,
                        mvar->canPutE))
      return 1;
    pr_cur = get_pendingReaders(mvar);
    if (pr_cur >= pr_prev) {
      pr_cur == 0;
      reset_pendingReaders(mvar);
    }
    pr_prev = pr_cur;
    if (pr_cur == 0) load_data(mvar, localDataPtr);
    ASSERT_TRUE(ReleaseMutex(mvar->guardM));
  } while (pr_cur != 0);
  return 0;
}

int mvar_trytake(MVar *mvar, void *localDataPtr) {
  int pr_prev = INT_MAX, pr_cur;
  do {
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
    if (pr_cur == 0) load_data(mvar, localDataPtr);
    ASSERT_TRUE(ReleaseMutex(mvar->guardM));
  } while (pr_cur != 0);
  return 0;
}

int mvar_put(MVar *mvar, void *localDataPtr, StgStablePtr tso) {
  STATE_IT("mvar_put", mvar);
  TIME_IT("mvar_put");
  if (!interpret_wait("mvar_put", mvar,
                      wait_interruptible(mvar, tso, &(mvar->guardM)), NULL,
                      mvar->canTakeE))
    return 1;
  save_data(mvar, localDataPtr);
  ASSERT_TRUE(ReleaseMutex(mvar->guardM));
  return 0;
}

int mvar_tryput(MVar *mvar, void *localDataPtr) {
  TIME_IT("mvar_tryput");
  if (!interpret_wait("mvar_tryput", mvar,
                      WaitForSingleObject(mvar->guardM, INFINITE),
                      mvar->canPutE, mvar->canTakeE))
    return 1;
  save_data(mvar, localDataPtr);
  ASSERT_TRUE(ReleaseMutex(mvar->guardM));
  return 0;
}

int mvar_read(MVar *mvar, void *localDataPtr, StgStablePtr tso) {
  STATE_IT("mvar_read", mvar);
  TIME_IT("mvar_read");
  DWORD r = WaitForSingleObject(mvar->guardM, INFINITE);
  TIME_IT("mvar_read/guarded");
  if (r == WAIT_OBJECT_0 || r == WAIT_ABANDONED_0) {
    TIME_IT("mvar_read/waiting");
    if (!is_signalled(mvar->canTakeE)) {
      {
        TIME_IT("mvar_read/waiting/inc");
        inc_pendingReaders(mvar);
        ASSERT_TRUE(ReleaseMutex(mvar->guardM));
      }
      {
        TIME_IT("mvar_read/waiting/main");
        r = wait_interruptible(mvar, tso, &(mvar->canTakeE));
      }
      {
        TIME_IT("mvar_read/waiting/dec");
        dec_pendingReaders(mvar);
      }
    }
  }
  if (!interpret_wait("mvar_read", mvar, r, NULL, mvar->canPutE)) return 1;
  memcpy(localDataPtr, mvar->dataPtr, mvar->statePtr->dataSize);
  ASSERT_TRUE(ReleaseMutex(mvar->guardM));
  return 0;
}

int mvar_tryread(MVar *mvar, void *localDataPtr) {
  TIME_IT("mvar_tryread");
  if (!interpret_wait("mvar_tryread", mvar,
                      WaitForSingleObject(mvar->guardM, INFINITE),
                      mvar->canTakeE, mvar->canPutE))
    return 1;
  memcpy(localDataPtr, mvar->dataPtr, mvar->statePtr->dataSize);
  ASSERT_TRUE(ReleaseMutex(mvar->guardM));
  return 0;
}

int mvar_swap(MVar *mvar, void *inPtr, void *outPtr, StgStablePtr tso) {
  TIME_IT("mvar_swap");
  if (!interpret_wait("mvar_swap", mvar,
                      wait_interruptible(mvar, tso, &(mvar->canTakeE)), NULL,
                      mvar->canPutE))
    return 1;
  load_data(mvar, outPtr);
  save_data(mvar, inPtr);
  ASSERT_TRUE(ReleaseMutex(mvar->guardM));
  return 0;
}

int mvar_tryswap(MVar *mvar, void *inPtr, void *outPtr) {
  TIME_IT("mvar_tryswap");
  if (!interpret_wait("mvar_tryswap", mvar,
                      WaitForSingleObject(mvar->guardM, INFINITE),
                      mvar->canTakeE, mvar->canPutE))
    return 1;
  load_data(mvar, outPtr);
  save_data(mvar, inPtr);
  ASSERT_TRUE(ReleaseMutex(mvar->guardM));
  return 0;
}

int mvar_isempty(MVar *mvar) { return is_signalled(mvar->canTakeE) ? 0 : 1; }

void mvar_name(MVar *mvar, char *const name) { strcpy(name, mvar->mvarName); }
