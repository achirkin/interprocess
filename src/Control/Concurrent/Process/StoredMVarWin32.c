#include <common.h>

#include <assert.h>
#include <errno.h>
#include <stdlib.h>
#include <windows.h>

#ifndef WAIT_OBJECT_1
#define WAIT_OBJECT_1 ((WAIT_OBJECT_0) + 1)
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

void mvar_name(MVar *mvar, char *const name) { strcpy(name, mvar->mvarName); }
