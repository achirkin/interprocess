#ifndef __HSSHAREDPTR_H__
#define __HSSHAREDPTR_H__

#include <stddef.h>
#include "HsFFI.h"
#include "MachDeps.h"
#include "SharedObjectName.h"

#if defined(WIN32) || defined(_WIN32) || defined(__WIN32) || defined(mingw32_HOST_OS)
#include <windows.h>
typedef struct SharedMutex {
  SharedObjectName mutexName;
} SharedMutex;
#else
#include <pthread.h>
typedef struct SharedMutex {
  pthread_mutex_t     mutVal;
  pthread_mutexattr_t mutAttr;
} SharedMutex;
#endif

typedef struct SharedAllocator SharedAllocator;
// Internal pointer representation is a single word;
typedef HsWord SharedPtr;

// Create a new memory allocator
//   Returns NULL on failure
SharedAllocator *shared_createAllocator();
// Create a new memory allocator for an existing memory pool
//   Returns NULL on failure
SharedAllocator *shared_lookupAllocator(const char *storeName);
// Destroy a memory allocator.
// The allocator keeps a number of its instances;
// If you create+lookup an allocator N times, you need to destroy it N times.
// Actual desctuction of shared data happens only on the last usage.
void  shared_destroyAllocator(SharedAllocator *aptr);
// Get shared store name, to pass it to another process and lookup an allocator.
char *shared_getStoreName(SharedAllocator *aptr);
// Convert a pointer to a tagged portable shared pointer.
// The result pointer cannot be used directly, but can be transfered between
// different memory spaces.
SharedPtr shared_ptrToShPtr(SharedAllocator *aptr, void     *ptr);
// May return NULL on failure.
void     *shared_shPtrToPtr(SharedAllocator *aptr, SharedPtr ptr);

// Allocate memory.
//  Memory is aligned to page size or closest bigger power of two to the allocation size.
//  Returns NULL on failure.
void *shared_malloc(SharedAllocator *aptr, size_t size);
void *shared_realloc(SharedAllocator *aptr, void *ptr, size_t size);
void  shared_free(SharedAllocator *aptr, void *ptr);




void _SharedMutex_init(SharedMutex *mptr, void **privateMutexHandle, const int createNew);
void _SharedMutex_destroy(SharedMutex *mptr, void **privateMutexHandle);
int _SharedMutex_lock(SharedMutex *mptr, void **privateMutexHandle);
int _SharedMutex_unlock(SharedMutex *mptr, void **privateMutexHandle);
HsPtr _store_alloc(const char *memBlockName, void **privateStoreHandle, size_t size);
void _store_free(const char *memBlockName, void **privateStoreHandle, HsPtr addr, size_t size, _Bool unlinkToo);


#endif /* __HSSHAREDPTR_H__ */
