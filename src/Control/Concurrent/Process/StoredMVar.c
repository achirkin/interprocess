#include "SharedObjectName.h"
#include "HsFFI.h"
#include <stdlib.h>
#include <assert.h>

typedef struct MVar MVar;

MVar *mvar_new(size_t byteSize);
MVar *mvar_lookup(const char *name);
void  mvar_destroy(MVar *mvar);
void  mvar_name(MVar *mvar, char * const name);

void mvar_take(MVar *mvar, void *localDataPtr);
int  mvar_trytake(MVar *mvar, void *localDataPtr);
void mvar_put(MVar *mvar, void *localDataPtr);
int  mvar_tryput(MVar *mvar, void *localDataPtr);
void mvar_read(MVar *mvar, void *localDataPtr);
int  mvar_tryread(MVar *mvar, void *localDataPtr);



#if defined(WIN32) || defined(_WIN32) || defined(__WIN32) || defined(mingw32_HOST_OS)


#else
#include "SharedPtr.h"
#include <fcntl.h>           /* For O_* constants */
#include <semaphore.h>
#include <string.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <unistd.h>

#include <stdio.h>
#include <time.h>
#include <errno.h>

typedef struct MVarState {
  int                 isFull;
  int                 pendingReaders;
  int                 totalUsers;
  pthread_mutex_t     mvMut;
  pthread_mutexattr_t mvMAttr;
  pthread_cond_t      canTakeC;
  pthread_cond_t      canPutC;
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
} MVar;

// ensure 64 byte alignment (maximum possible we can think of, e.g. AVX512)
size_t mvar_state_size64() {
  size_t x = sizeof(MVarState);
  size_t r = x % 64;
  return r == 0 ? x : (x + 64 - r);
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
  r->statePtr = (MVarState*) _store_alloc(r->mvarName, NULL, totalSize);
  if (r->statePtr == NULL) {
    free(r);
    return NULL;
  }
  r->dataPtr = ((void*)(r->statePtr)) + dataShift;

  // setup state
  MVarState s = (struct MVarState)
    { .isFull = 0
    , .pendingReaders = 0
    , .totalUsers = 1
    , .dataSize = byteSize
    };
  *(r->statePtr) = s;
  pthread_mutexattr_init(&(s.mvMAttr));
#ifndef NDEBUG
  pthread_mutexattr_settype(&(s.mvMAttr), PTHREAD_MUTEX_ERRORCHECK);
#endif
  // pthread_mutexattr_settype(&(s.mvMAttr), PTHREAD_MUTEX_ROBUST);
  pthread_mutexattr_setpshared(&(s.mvMAttr), PTHREAD_PROCESS_SHARED);
  pthread_mutex_init(&(s.mvMut), &(s.mvMAttr));
  pthread_condattr_init(&(s.condAttr));
  pthread_condattr_setpshared(&(s.condAttr), PTHREAD_PROCESS_SHARED);
  pthread_cond_init(&(s.canTakeC), &(s.condAttr));
  pthread_cond_init(&(s.canPutC), &(s.condAttr));

  return r;
}

MVar *mvar_lookup(const char *name) {
  int memFd = shm_open(name, O_RDWR, S_IRWXU);
  if (memFd < 0) {
    return NULL;
  }
  // first, map only sizeof(MVarState) bytes
  // then, read the actual size and remap memory
  HsPtr m0 = mmap( NULL
                 , sizeof(MVarState)
                 , PROT_READ | PROT_WRITE
                 , MAP_SHARED
                 , memFd, 0);
  if (m0 == MAP_FAILED) {
    return NULL;
  }
  size_t dataShift = mvar_state_size64(),
         storeSize = dataShift + ((MVarState*)m0)->dataSize;
  HsPtr m = mremap(m0, 0, storeSize, MREMAP_MAYMOVE);
  if (m == MAP_FAILED) {
    munmap(m0, sizeof(MVarState));
    return NULL;
  }
  // setup MVar struct
  MVar *r = malloc(sizeof(MVar));
  if (r == NULL) {
    munmap(m, storeSize);
    return NULL;
  }
  r->statePtr = (MVarState*)m;
  r->dataPtr  = ((void*)(r->statePtr)) + mvar_state_size64();
  strcpy(r->mvarName, name);
  // update state
  printf("lookup - locking\n");
  int r0 = pthread_mutex_lock(&(r->statePtr->mvMut));
  printf("lookup - locked, %d\n", r0);
  if ( r->statePtr->totalUsers == 0 ) {
    r0 = pthread_mutex_unlock(&(r->statePtr->mvMut));
    printf("lookup - emergency unlocked, %d\n", r0);
    munmap(m, storeSize);
    free(r);
    return NULL;
  }
  assert( r0 == 0 );
  r->statePtr->totalUsers++;
  r0 = pthread_mutex_unlock(&(r->statePtr->mvMut));
  printf("lookup - unlocked, %d\n", r0);
  assert( r0 == 0 );

  return r;
}

void mvar_destroy(MVar *mvar) {
  printf("destroy - starting...\n");
  int r = pthread_mutex_lock(&(mvar->statePtr->mvMut));
  printf("destroy - locked, %d\n", r);
  assert( r == 0 );
  int usersN = --(mvar->statePtr->totalUsers);
  r = pthread_mutex_unlock(&(mvar->statePtr->mvMut));
  assert( r == 0 );
  printf("destroy - unlocked, %d\n", r);
  size_t storeSize = mvar->statePtr->dataSize + mvar_state_size64();
  if (usersN > 0) {
    // pthread_cond_broadcast(&(mvar->statePtr->canTakeC));
    // pthread_cond_broadcast(&(mvar->statePtr->canPutC));
    printf("destroy... users left: %d\n", usersN);
    munmap(mvar->statePtr, storeSize);
  } else {
    printf("destroy... for real!\n");
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


void mvar_take(MVar *mvar, void *localDataPtr) {
  printf("take - entering\n");
  // wait
  int r = pthread_mutex_lock(&(mvar->statePtr->mvMut));
  assert( r == 0 );
  printf("take - locked, isFull: %d, pr %d\n", mvar->statePtr->isFull, mvar->statePtr->pendingReaders);
  while ( !(mvar->statePtr->isFull) || (mvar->statePtr->pendingReaders > 0) ) {
    printf("take - pthread_cond_wait iteration, isFull: %d, pr %d\n", mvar->statePtr->isFull, mvar->statePtr->pendingReaders);
    r = pthread_cond_wait(&(mvar->statePtr->canTakeC), &(mvar->statePtr->mvMut));
    assert( r == 0 );
  }
  printf("take - copying\n");
  // copy data
  memcpy(localDataPtr, mvar->dataPtr, mvar->statePtr->dataSize);
  // mark empty
  mvar->statePtr->isFull = 0;
  // wakeup at least one putter
  printf("take - pthread_cond_signal\n");
  r = pthread_cond_signal(&(mvar->statePtr->canPutC));
  assert( r == 0 );
  printf("take - unlocking\n");
  r = pthread_mutex_unlock(&(mvar->statePtr->mvMut));
  assert( r == 0 );
  printf("take - finished!\n");
}

int mvar_trytake(MVar *mvar, void *localDataPtr) {
  int r = pthread_mutex_lock(&(mvar->statePtr->mvMut));
  assert( r == 0 );
  if ( !(mvar->statePtr->isFull) ) {
    r = pthread_mutex_unlock(&(mvar->statePtr->mvMut));
    assert( r == 0 );
    return 1;
  }
  while ( mvar->statePtr->pendingReaders > 0 ) {
    r = pthread_cond_wait(&(mvar->statePtr->canTakeC), &(mvar->statePtr->mvMut));
    assert( r == 0 );
  }
  // copy data
  memcpy(localDataPtr, mvar->dataPtr, mvar->statePtr->dataSize);
  // mark empty
  mvar->statePtr->isFull = 0;
  r = pthread_cond_signal(&(mvar->statePtr->canPutC));
  assert( r == 0 );
  // wakeup at least one putter
  r = pthread_mutex_unlock(&(mvar->statePtr->mvMut));
  assert( r == 0 );
  return 0;
}


void mvar_put(MVar *mvar, void *localDataPtr) {
  printf("put - entering\n");
  int r = pthread_mutex_lock(&(mvar->statePtr->mvMut));
  assert( r == 0 );
  printf("put - locked, isFull: %d\n", mvar->statePtr->isFull);
  while ( mvar->statePtr->isFull ) {
    printf("put - pthread_cond_wait iteration, isFull: %d\n", mvar->statePtr->isFull);
    r = pthread_cond_wait(&(mvar->statePtr->canPutC), &(mvar->statePtr->mvMut));
    assert( r == 0 );
  }
  printf("put - copying\n");
  // copy data
  memcpy(mvar->dataPtr, localDataPtr, mvar->statePtr->dataSize);
  // mark full
  mvar->statePtr->isFull = 1;
  // wakeup all readers!
  printf("put - pthread_cond_broadcast\n");
  r = pthread_cond_broadcast(&(mvar->statePtr->canTakeC));
  assert( r == 0 );
  printf("put - unlocking\n");
  r = pthread_mutex_unlock(&(mvar->statePtr->mvMut));
  assert( r == 0 );
  printf("put - finished!\n");
}

int mvar_tryput(MVar *mvar, void *localDataPtr) {
  int r = pthread_mutex_lock(&(mvar->statePtr->mvMut));
  assert( r == 0 );
  if ( mvar->statePtr->isFull ) {
    r = pthread_mutex_unlock(&(mvar->statePtr->mvMut));
    assert( r == 0 );
    return 1;
  }
  // copy data
  memcpy(mvar->dataPtr, localDataPtr, mvar->statePtr->dataSize);
  // mark full
  mvar->statePtr->isFull = 1;
  // wakeup all readers!
  r = pthread_cond_broadcast(&(mvar->statePtr->canTakeC));
  assert( r == 0 );
  r = pthread_mutex_unlock(&(mvar->statePtr->mvMut));
  assert( r == 0 );
  return 0;
}


void mvar_read(MVar *mvar, void *localDataPtr) {
  int r = pthread_mutex_lock(&(mvar->statePtr->mvMut));
  assert( r == 0 );
  // I am waiting!
  mvar->statePtr->pendingReaders++;
  while ( !(mvar->statePtr->isFull) ) {
    r = pthread_cond_wait(&(mvar->statePtr->canTakeC), &(mvar->statePtr->mvMut));
    assert( r == 0 );
  }
  // copy data
  memcpy(localDataPtr, mvar->dataPtr, mvar->statePtr->dataSize);
  // the last reader should wakeup at least one taker
  // Decrement the waiters counter and maybe wake up another taker
  if ( (--(mvar->statePtr->pendingReaders)) == 0 ) {
    r = pthread_cond_signal(&(mvar->statePtr->canTakeC));
    assert( r == 0 );
  }
  r = pthread_mutex_unlock(&(mvar->statePtr->mvMut));
  assert( r == 0 );
}

int mvar_tryread(MVar *mvar, void *localDataPtr) {
  int r = pthread_mutex_lock(&(mvar->statePtr->mvMut));
  assert( r == 0 );
  if ( !(mvar->statePtr->isFull) ) {
    r = pthread_mutex_unlock(&(mvar->statePtr->mvMut));
    assert( r == 0 );
    return 1;
  } else {
    memcpy(localDataPtr, mvar->dataPtr, mvar->statePtr->dataSize);
    r = pthread_mutex_unlock(&(mvar->statePtr->mvMut));
    assert( r == 0 );
    return 0;
  }
}


void mvar_name(MVar *mvar, char * const name) {
  strcpy(name, mvar->mvarName);
}

#endif
