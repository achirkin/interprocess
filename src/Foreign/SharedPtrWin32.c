#include "SharedPtr.h"

void _SharedMutex_init(SharedMutex *mptr, void **privateMutexHandle,
                       const int createNew) {
  if (createNew != 0) {
    genSharedObjectName(mptr->mutexName);
  }
  *privateMutexHandle =
      CreateMutexA(/** default security attributes*/ NULL,
                   /** initially not owned */ FALSE, mptr->mutexName);
#ifdef INTERPROCESS_DEBUG
  if (*privateMutexHandle == NULL) {
    INTERPROCESS_LOG_DEBUG("CreateMutex error: %d\n", GetLastError());
  }
#endif
}

void _SharedMutex_destroy(SharedMutex *mptr, void **privateMutexHandle) {
  CloseHandle(*privateMutexHandle);
}

int _SharedMutex_lock(SharedMutex *mptr, void **privateMutexHandle) {
  DWORD r = WaitForSingleObject(*privateMutexHandle, INFINITE);
  if (r != WAIT_OBJECT_0) {
    INTERPROCESS_LOG_DEBUG(
        "WaitForSingleObject mutex error: return %d; error code %d.\n", r,
        GetLastError());
    return 1;
  } else {
    return 0;
  }
}

int _SharedMutex_unlock(SharedMutex *mptr, void **privateMutexHandle) {
  DWORD r = ReleaseMutex(*privateMutexHandle);
  if (r == 0) {
    INTERPROCESS_LOG_DEBUG("ReleaseMutex error: %d\n", GetLastError());
    return 1;
  } else {
    return 0;
  }
}

// returns NULL if failed
HsPtr _store_alloc(const char *memBlockName, void **privateMutexHandle,
                   size_t size) {
  *privateMutexHandle = CreateFileMappingA(
      /** use paging file */ INVALID_HANDLE_VALUE,
      /** default security */ NULL,
      /** read/write access */ PAGE_READWRITE,
      /** maximum object size (high-order DWORD) */ (DWORD)(size >> 32),
      /** maximum object size (low-order DWORD) */ (DWORD)(size & 0xFFFFFFFF),
      /** name of mapping object */ memBlockName);

  if (*privateMutexHandle == NULL) {
    INTERPROCESS_LOG_DEBUG("Could not create file mapping object (%d).\n",
                           GetLastError());
    return NULL;
  }
  void *rptr = MapViewOfFile(/** handle to map object */ *privateMutexHandle,
                             /** read/write permission */ FILE_MAP_ALL_ACCESS,
                             0, 0, size);

  if (rptr == NULL) {
    INTERPROCESS_LOG_DEBUG("Could not map view of file (%d).\n",
                           GetLastError());
    CloseHandle(*privateMutexHandle);
    return NULL;
  }

  return rptr;
}

// failures of system calls are ignored
void _store_free(const char *memBlockName, void **privateStoreHandle,
                 HsPtr addr, size_t size, _Bool unlinkToo) {
  if (addr != 0) {
    UnmapViewOfFile(addr);
    CloseHandle(*privateStoreHandle);
  }
}
