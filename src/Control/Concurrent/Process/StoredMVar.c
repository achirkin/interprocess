#include "SharedObjectName.h"
#include "HsFFI.h"
#include <stdlib.h>

typedef struct MVar MVar;

MVar *mvar_new(size_t byteSize);
MVar *mvar_lookup(const char *name);
void  mvar_destroy(MVar *mvar);
void  mvar_name(MVar *mvar, char * const name);


int mvar_take   (MVar *mvar, void *localDataPtr);
int mvar_trytake(MVar *mvar, void *localDataPtr);
int mvar_put    (MVar *mvar, void *localDataPtr);
int mvar_tryput (MVar *mvar, void *localDataPtr);
int mvar_read   (MVar *mvar, void *localDataPtr);
int mvar_tryread(MVar *mvar, void *localDataPtr);
int mvar_swap   (MVar *mvar, void *inPtr, void *outPtr);
int mvar_tryswap(MVar *mvar, void *inPtr, void *outPtr);
int mvar_isempty(MVar *mvar);



#if defined(WIN32) || defined(_WIN32) || defined(__WIN32) || defined(mingw32_HOST_OS)


#else
#include <pthread.h>
#include <fcntl.h>
#include <string.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <unistd.h>

#include <stdio.h>
#include <time.h>
#include <errno.h>
#include <signal.h>

typedef struct MVarState {
  pthread_mutex_t     mvMut;
  pthread_cond_t      canPutC;
  pthread_cond_t      canTakeC;
  size_t              dataSize;
  pthread_mutexattr_t mvMAttr;
  pthread_condattr_t  condAttr;
  int                 isFull;
  int                 pendingReaders;
  int                 totalUsers;
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
  mvar->statePtr = (MVarState*) mmap( NULL, totalSize
                                    , PROT_READ | PROT_WRITE
                                    , MAP_SHARED, memFd, 0);
  if (mvar->statePtr == MAP_FAILED) {
    shm_unlink(mvar->mvarName);
    free(mvar);
    return NULL;
  }
  mvar->dataPtr = ((void*)(mvar->statePtr)) + dataShift;

  // setup state
  *(mvar->statePtr) = (struct MVarState)
    { .isFull = 0
    , .pendingReaders = 0
    , .dataSize = byteSize
    , .totalUsers = 1
    };

  // init mutex and condition variables
  r = pthread_mutexattr_init(&(mvar->statePtr->mvMAttr));
  //if ( r == 0) r = pthread_mutexattr_settype(&(mvar->statePtr->mvMAttr), PTHREAD_MUTEX_ERRORCHECK);
  if ( r == 0) r = pthread_mutexattr_setpshared(&(mvar->statePtr->mvMAttr), PTHREAD_PROCESS_SHARED);
  if ( r == 0) r = pthread_mutex_init(&(mvar->statePtr->mvMut), &(mvar->statePtr->mvMAttr));
  if ( r == 0) r = pthread_condattr_init(&(mvar->statePtr->condAttr));
  if ( r == 0) r = pthread_condattr_setpshared(&(mvar->statePtr->condAttr), PTHREAD_PROCESS_SHARED);
  if ( r == 0) r = pthread_cond_init(&(mvar->statePtr->canPutC), &(mvar->statePtr->condAttr));
  if ( r == 0) r = pthread_cond_init(&(mvar->statePtr->canTakeC), &(mvar->statePtr->condAttr));
  if ( r != 0 ) {
    munmap(mvar->statePtr, totalSize);
    shm_unlink(mvar->mvarName);
    free(mvar);
    return NULL;
  }

  return mvar;
}

MVar *mvar_lookup(const char *name) {
  int memFd = shm_open(name, O_RDWR, S_IRWXU);
  if (memFd < 0) return NULL;

  // first, map only sizeof(MVarState) bytes
  // then, read the actual size and remap memory
  void *mvs0 = mmap( NULL, sizeof(MVarState)
                   , PROT_READ | PROT_WRITE | PROT_EXEC
                   , MAP_SHARED, memFd, 0);
  if (mvs0 == MAP_FAILED) return NULL;
  size_t dataShift = mvar_state_size64(),
         storeSize = dataShift + ((MVarState*)mvs0)->dataSize;
  MVarState *mvs = (MVarState*) mremap(mvs0, 0, storeSize, MREMAP_MAYMOVE);
  munmap(mvs0, sizeof(MVarState)); // don't really care if it is failed
  if (mvs == MAP_FAILED) return NULL;

  // setup MVar struct
  MVar *mvar = malloc(sizeof(MVar));
  if (mvar == NULL) {
    munmap(mvs, storeSize);
    return NULL;
  }
  mvar->statePtr = (MVarState*)mvs;
  mvar->dataPtr  = ((void*)(mvar->statePtr)) + dataShift;
  strcpy(mvar->mvarName, name);

  // update state
  int r = 0;
  r = pthread_mutex_lock(&(mvar->statePtr->mvMut));
  if ( r != 0 || mvar->statePtr->totalUsers == 0 ) {
    munmap(mvar->statePtr, storeSize);
    free(mvar);
    return NULL;
  }
  mvar->statePtr->totalUsers++;
  r = pthread_mutex_unlock(&(mvar->statePtr->mvMut));
  if ( r != 0 ) {
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
  if ( usersLeft > 0 ) {
#ifndef NDEBUG
    printf( "Destroying local instance of mvar %s, %d users left.\n"
          , mvar->mvarName, usersLeft);
#endif
    munmap(mvar->statePtr, storeSize);
  } else {
#ifndef NDEBUG
    printf( "Destroying mvar %s globally (no other users left).\n", mvar->mvarName);
#endif
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

void mvar_name(MVar *mvar, char * const name) {
  strcpy(name, mvar->mvarName);
}


int mvar_take(MVar *mvar, void *localDataPtr) {
  int r = 0;
  r = pthread_mutex_lock(&(mvar->statePtr->mvMut));
  if ( r != 0 ) return r;
  while ( !(mvar->statePtr->isFull) || mvar->statePtr->pendingReaders > 0 ) {
    if ( mvar->statePtr->pendingReaders > 0 ) {
      pthread_cond_broadcast(&(mvar->statePtr->canTakeC));
    }
    r = pthread_cond_wait(&(mvar->statePtr->canTakeC), &(mvar->statePtr->mvMut));
    if ( r != 0 ) {
      pthread_mutex_unlock(&(mvar->statePtr->mvMut));
      return r;
    }
  }
  memcpy(localDataPtr, mvar->dataPtr, mvar->statePtr->dataSize);
  mvar->statePtr->isFull = 0;
  pthread_cond_signal(&(mvar->statePtr->canPutC));
  pthread_mutex_unlock(&(mvar->statePtr->mvMut));
  return 0;
}

int mvar_trytake(MVar *mvar, void *localDataPtr) {
  int r = 0;
  r = pthread_mutex_lock(&(mvar->statePtr->mvMut));
  if ( r != 0 ) return r;
  // shortcut if is empty
  if ( !(mvar->statePtr->isFull) ) {
    pthread_mutex_unlock(&(mvar->statePtr->mvMut));
    return 1;
  }
  while ( mvar->statePtr->pendingReaders > 0 ) {
    // make sure readers do not sleep
    pthread_cond_broadcast(&(mvar->statePtr->canTakeC));
    // last reader should wake me up, wait for it.
    r = pthread_cond_wait(&(mvar->statePtr->canTakeC), &(mvar->statePtr->mvMut));
    if ( r != 0 ) {
      pthread_mutex_unlock(&(mvar->statePtr->mvMut));
      return r;
    }
    // repeat emptyness check (if another trytake was faster)
    if ( !(mvar->statePtr->isFull) ) {
      pthread_mutex_unlock(&(mvar->statePtr->mvMut));
      return 1;
    }
  }
  memcpy(localDataPtr, mvar->dataPtr, mvar->statePtr->dataSize);
  mvar->statePtr->isFull = 0;
  pthread_cond_signal(&(mvar->statePtr->canPutC));
  pthread_mutex_unlock(&(mvar->statePtr->mvMut));
  return 0;
}

int mvar_put(MVar *mvar, void *localDataPtr) {
  int r = 0;
  r = pthread_mutex_lock(&(mvar->statePtr->mvMut));
  if ( r != 0 ) return r;
  while ( mvar->statePtr->isFull ) {
    r = pthread_cond_wait(&(mvar->statePtr->canPutC), &(mvar->statePtr->mvMut));
    if ( r != 0 ) {
      pthread_mutex_unlock(&(mvar->statePtr->mvMut));
      return r;
    }
  }
  memcpy(mvar->dataPtr, localDataPtr, mvar->statePtr->dataSize);
  mvar->statePtr->isFull = 1;
  pthread_cond_broadcast(&(mvar->statePtr->canTakeC));
  pthread_mutex_unlock(&(mvar->statePtr->mvMut));
  return 0;
}

int mvar_tryput(MVar *mvar, void *localDataPtr) {
  int r = 0;
  r = pthread_mutex_lock(&(mvar->statePtr->mvMut));
  if ( r != 0 ) return r;
  // shortcut if is full
  if ( mvar->statePtr->isFull ) {
    pthread_mutex_unlock(&(mvar->statePtr->mvMut));
    return 1;
  }
  memcpy(mvar->dataPtr, localDataPtr, mvar->statePtr->dataSize);
  mvar->statePtr->isFull = 1;
  pthread_cond_broadcast(&(mvar->statePtr->canTakeC));
  pthread_mutex_unlock(&(mvar->statePtr->mvMut));
  return 0;
}

int mvar_read(MVar *mvar, void *localDataPtr) {
  int r = 0;
  r = pthread_mutex_lock(&(mvar->statePtr->mvMut));
  if ( r != 0 ) return r;
  mvar->statePtr->pendingReaders++;
  while ( !(mvar->statePtr->isFull) ) {
    r = pthread_cond_wait(&(mvar->statePtr->canTakeC), &(mvar->statePtr->mvMut));
    if ( r != 0 ) {
      pthread_mutex_unlock(&(mvar->statePtr->mvMut));
      return r;
    }
  }
  memcpy(localDataPtr, mvar->dataPtr, mvar->statePtr->dataSize);
  if ( (--(mvar->statePtr->pendingReaders)) == 0 ) {
    pthread_cond_signal(&(mvar->statePtr->canTakeC));
  }
  pthread_mutex_unlock(&(mvar->statePtr->mvMut));
  return 0;
}

int mvar_tryread(MVar *mvar, void *localDataPtr) {
  int r = 0;
  r = pthread_mutex_lock(&(mvar->statePtr->mvMut));
  if ( r != 0 ) return r;
  // shortcut if is empty
  if ( !(mvar->statePtr->isFull) ) {
    pthread_mutex_unlock(&(mvar->statePtr->mvMut));
    return 1;
  }
  memcpy(localDataPtr, mvar->dataPtr, mvar->statePtr->dataSize);
  pthread_mutex_unlock(&(mvar->statePtr->mvMut));
  return 0;
}

int mvar_swap(MVar *mvar, void *inPtr, void *outPtr) {
  int r = 0;
  r = pthread_mutex_lock(&(mvar->statePtr->mvMut));
  if ( r != 0 ) return r;
  while ( !(mvar->statePtr->isFull) || mvar->statePtr->pendingReaders > 0 ) {
    if ( mvar->statePtr->pendingReaders > 0 ) {
      pthread_cond_broadcast(&(mvar->statePtr->canTakeC));
    }
    r = pthread_cond_wait(&(mvar->statePtr->canTakeC), &(mvar->statePtr->mvMut));
    if ( r != 0 ) {
      pthread_mutex_unlock(&(mvar->statePtr->mvMut));
      return r;
    }
  }
  memcpy(outPtr, mvar->dataPtr, mvar->statePtr->dataSize);
  memcpy(mvar->dataPtr, inPtr , mvar->statePtr->dataSize);
  pthread_cond_signal(&(mvar->statePtr->canTakeC));
  pthread_mutex_unlock(&(mvar->statePtr->mvMut));
  return 0;
}

int mvar_tryswap(MVar *mvar, void *inPtr, void *outPtr) {
  int r = 0;
  r = pthread_mutex_lock(&(mvar->statePtr->mvMut));
  if ( r != 0 ) return r;
  // shortcut if is empty
  if ( !(mvar->statePtr->isFull) ) {
    pthread_mutex_unlock(&(mvar->statePtr->mvMut));
    return 1;
  }
  while ( mvar->statePtr->pendingReaders > 0 ) {
    // make sure readers do not sleep
    pthread_cond_broadcast(&(mvar->statePtr->canTakeC));
    // last reader should wake me up, wait for it.
    r = pthread_cond_wait(&(mvar->statePtr->canTakeC), &(mvar->statePtr->mvMut));
    if ( r != 0 ) {
      pthread_mutex_unlock(&(mvar->statePtr->mvMut));
      return r;
    }
    // repeat emptyness check (if another trytake was faster)
    if ( !(mvar->statePtr->isFull) ) {
      pthread_mutex_unlock(&(mvar->statePtr->mvMut));
      return 1;
    }
  }
  memcpy(outPtr, mvar->dataPtr, mvar->statePtr->dataSize);
  memcpy(mvar->dataPtr, inPtr , mvar->statePtr->dataSize);
  pthread_cond_signal(&(mvar->statePtr->canPutC));
  pthread_mutex_unlock(&(mvar->statePtr->mvMut));
  return 0;
}

int mvar_isempty(MVar *mvar) {
  return mvar->statePtr->isFull == 0;
}



#endif
