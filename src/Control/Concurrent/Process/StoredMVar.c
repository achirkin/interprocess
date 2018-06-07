#include "SharedObjectName.h"
#include "HsFFI.h"
#include <stdlib.h>
#include <assert.h>

typedef struct MVar MVar;

MVar *mvar_new(size_t byteSize);
MVar *mvar_lookup(const char *name);
void  mvar_destroy(MVar *mvar);
void  mvar_name(MVar *mvar, char * const name);
void *mvar_dataptr(MVar *mvar);

/* All mvar_xxxstart functions lock MVar state on success
   and do not lock it on failure.

   mvar_tryxxxstart functions return 1 on failure and 0 on success.
 */

// acquire and lock mvar, wait until can take it
void mvar_takestart(MVar *mvar);
int  mvar_trytakestart(MVar *mvar);
// release mvar, set its state to full, and wakeup at least one putter
void mvar_takeonsuccess(MVar *mvar);
// acquire and lock mvar, wait until can put it
void mvar_putstart(MVar *mvar);
int  mvar_tryputstart(MVar *mvar);
// release mvar, set its state to empty, and wakeup all getters
void mvar_putonsuccess(MVar *mvar);
// acquire and lock mvar, wait until can read it, increment reader counter
void mvar_readstart(MVar *mvar);
int  mvar_tryreadstart(MVar *mvar);
// release mvar, decrement reader counter, and maybe wakeup at least one taker
void mvar_readonsuccess(MVar *mvar);


#if defined(WIN32) || defined(_WIN32) || defined(__WIN32) || defined(mingw32_HOST_OS)


#else
#include "SharedPtr.h"
#include <fcntl.h>           /* For O_* constants */
#include <semaphore.h>
#include <string.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <unistd.h>

typedef struct MVarState {
  int                 isFull;
  int                 pendingReaders;
  int                 totalUsers;
  pthread_mutex_t     mvMut;
  pthread_mutexattr_t mvMAttr;
  pthread_cond_t      canTakeC;
  pthread_cond_t      canPutC;
  size_t              storeSize;
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
    , .storeSize = totalSize
    };
  *(r->statePtr) = s;
  pthread_mutexattr_init(&(s.mvMAttr));
  pthread_mutexattr_settype(&(s.mvMAttr), PTHREAD_MUTEX_ERRORCHECK);
  pthread_mutexattr_setpshared(&(s.mvMAttr), PTHREAD_PROCESS_SHARED);
  pthread_mutex_init(&(s.mvMut), &(s.mvMAttr));
  pthread_cond_init(&(s.canTakeC), NULL);
  pthread_cond_init(&(s.canPutC), NULL);

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
  HsPtr m = mremap(m0, 0, ((MVarState*)m0)->storeSize, MREMAP_MAYMOVE);
  if (m == MAP_FAILED) {
    munmap(m0, sizeof(MVarState));
    return NULL;
  }
  // setup MVar struct
  MVar *r = malloc(sizeof(MVar));
  if (r == NULL) {
    munmap(m, ((MVarState*)m)->storeSize);
    return NULL;
  }
  r->statePtr = (MVarState*)m;
  r->dataPtr  = ((void*)(r->statePtr)) + mvar_state_size64();
  strcpy(r->mvarName, name);
  // update state
  pthread_mutex_lock(&(r->statePtr->mvMut));
  r->statePtr->totalUsers++;
  pthread_mutex_unlock(&(r->statePtr->mvMut));

  return r;
}

void mvar_destroy(MVar *mvar) {
  pthread_mutex_lock(&(mvar->statePtr->mvMut));
  int usersN = --(mvar->statePtr->totalUsers);
  pthread_mutex_unlock(&(mvar->statePtr->mvMut));
  if (usersN > 0) {
    munmap(mvar->statePtr, mvar->statePtr->storeSize);
  } else {
    pthread_cond_destroy(&(mvar->statePtr->canTakeC));
    pthread_cond_destroy(&(mvar->statePtr->canPutC));
    pthread_mutex_destroy(&(mvar->statePtr->mvMut));
    pthread_mutexattr_destroy(&(mvar->statePtr->mvMAttr));
    munmap(mvar->statePtr, mvar->statePtr->storeSize);
    shm_unlink(mvar->mvarName);
  }
  free(mvar);
}


void mvar_takestart(MVar *mvar) {
  int r = pthread_mutex_lock(&(mvar->statePtr->mvMut));
  assert( r == 0 );
  while ( !(mvar->statePtr->isFull) || (mvar->statePtr->pendingReaders > 0) ) {
    r = pthread_cond_wait(&(mvar->statePtr->canTakeC), &(mvar->statePtr->mvMut));
    assert( r == 0 );
  }
}

int mvar_trytakestart(MVar *mvar) {
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
  return 0;
}

void mvar_takeonsuccess(MVar *mvar) {
  mvar->statePtr->isFull = 0;
  // wakeup at least one putter
  int r = pthread_cond_signal(&(mvar->statePtr->canPutC));
  assert( r == 0 );
  r = pthread_mutex_unlock(&(mvar->statePtr->mvMut));
  assert( r == 0 );
}

void mvar_putstart(MVar *mvar) {
  int r = pthread_mutex_lock(&(mvar->statePtr->mvMut));
  assert( r == 0 );
  while ( mvar->statePtr->isFull ) {
    r = pthread_cond_wait(&(mvar->statePtr->canPutC), &(mvar->statePtr->mvMut));
    assert( r == 0 );
  }
}

int mvar_tryputstart(MVar *mvar) {
  int r = pthread_mutex_lock(&(mvar->statePtr->mvMut));
  assert( r == 0 );
  if ( mvar->statePtr->isFull ) {
    r = pthread_mutex_unlock(&(mvar->statePtr->mvMut));
    assert( r == 0 );
    return 1;
  } else
    return 0;
}

void mvar_putonsuccess(MVar *mvar) {
  mvar->statePtr->isFull = 1;
  // wakeup all readers!
  int r = pthread_cond_broadcast(&(mvar->statePtr->canTakeC));
  assert( r == 0 );
  r = pthread_mutex_unlock(&(mvar->statePtr->mvMut));
  assert( r == 0 );
}

void mvar_readstart(MVar *mvar) {
  int r = pthread_mutex_lock(&(mvar->statePtr->mvMut));
  assert( r == 0 );
  mvar->statePtr->pendingReaders++;
  while ( !(mvar->statePtr->isFull) ) {
    r = pthread_cond_wait(&(mvar->statePtr->canTakeC), &(mvar->statePtr->mvMut));
    assert( r == 0 );
  }
}

int mvar_tryreadstart(MVar *mvar) {
  int r = pthread_mutex_lock(&(mvar->statePtr->mvMut));
  assert( r == 0 );
  if ( !(mvar->statePtr->isFull) ) {
    r = pthread_mutex_unlock(&(mvar->statePtr->mvMut));
    assert( r == 0 );
    return 1;
  } else {
    mvar->statePtr->pendingReaders++;
    return 0;
  }
}

void mvar_readonsuccess(MVar *mvar) {
  // the last reader should wakeup at least one taker
  int r = 0;
  if ( (--(mvar->statePtr->pendingReaders)) == 0 ) {
    r = pthread_cond_signal(&(mvar->statePtr->canTakeC));
    assert( r == 0 );
  }
  r = pthread_mutex_unlock(&(mvar->statePtr->mvMut));
  assert( r == 0 );
}

void mvar_name(MVar *mvar, char * const name) {
  strcpy(name, mvar->mvarName);
}

void *mvar_dataptr(MVar *mvar) {
  return mvar->dataPtr;
}

#endif
