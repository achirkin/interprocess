#include "SharedPtr.h"
#ifdef STDOUT_SYSCALL_DEBUG
#include <stdio.h>
#include <tchar.h>
#endif

void _SharedMutex_init(SharedMutex *mptr, void **privateMutexHandle, const int createNew) {
  if (createNew != 0) {
    genSharedObjectName(mptr->mutexName);
  }
  *privateMutexHandle = CreateMutexA
    ( NULL    // default security attributes
    , FALSE   // initially not owned
    , mptr->mutexName );
#ifdef STDOUT_SYSCALL_DEBUG
  if (*privateMutexHandle == NULL) {
    _tprintf(TEXT("CreateMutex error: %d\n"), GetLastError());
  }
#endif
}

void _SharedMutex_destroy(SharedMutex *mptr, void **privateMutexHandle) {
  CloseHandle(*privateMutexHandle);
}

int _SharedMutex_lock(SharedMutex *mptr, void **privateMutexHandle) {
  DWORD r = WaitForSingleObject(*privateMutexHandle, INFINITE);
  if (r != WAIT_OBJECT_0) {
#ifdef STDOUT_SYSCALL_DEBUG
    _tprintf(TEXT("WaitForSingleObject mutex error: return %d; error code %d.\n"), r, GetLastError());
#endif
    return 1;
  } else {
    return 0;
  }
}

int _SharedMutex_unlock(SharedMutex *mptr, void **privateMutexHandle) {
  DWORD r = ReleaseMutex(*privateMutexHandle);
  if(r == 0) {
#ifdef STDOUT_SYSCALL_DEBUG
    _tprintf(TEXT("ReleaseMutex error: %d\n"), GetLastError());
#endif
	  return 1;
  } else {
	  return 0;
  }
}

// returns NULL if failed
HsPtr _store_alloc(const char *memBlockName, void **privateMutexHandle, size_t size){
  *privateMutexHandle = CreateFileMappingA
    ( INVALID_HANDLE_VALUE       // use paging file
    , NULL                       // default security
    , PAGE_READWRITE             // read/write access
    , (DWORD)(size >> 32)        // maximum object size (high-order DWORD)
    , (DWORD)(size & 0xFFFFFFFF) // maximum object size (low-order DWORD)
    , memBlockName);             // name of mapping object

  if (*privateMutexHandle == NULL) {
#ifdef STDOUT_SYSCALL_DEBUG
    _tprintf(TEXT("Could not create file mapping object (%d).\n"), GetLastError());
#endif
    return NULL;
  }
  void *rptr = MapViewOfFile
    ( *privateMutexHandle // handle to map object
    , FILE_MAP_ALL_ACCESS // read/write permission
    , 0
    , 0
    , size);

  if (rptr == NULL) {
#ifdef STDOUT_SYSCALL_DEBUG
	  _tprintf(TEXT("Could not map view of file (%d).\n"), GetLastError());
#endif
    CloseHandle(*privateMutexHandle);
    return NULL;
  }

  return rptr;
}

// failures of system calls are ignored
void _store_free( const char *memBlockName, void **privateStoreHandle, HsPtr addr
                , size_t size, _Bool unlinkToo) {
  if(addr != 0){
    UnmapViewOfFile(addr);
    CloseHandle(*privateStoreHandle);
  }
}
