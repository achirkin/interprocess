#include "SharedPtr.h"

#include <sys/mman.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>


void _SharedMutex_init(SharedMutex *mptr, void **privateMutexHandle, const int createNew) {
  *privateMutexHandle = (void*)mptr;
  if (createNew != 0) {
    pthread_mutexattr_init(&(mptr->mutAttr));
    // pthread_mutexattr_settype(&(mptr->mutAttr), PTHREAD_MUTEX_ERRORCHECK);
    pthread_mutexattr_setpshared(&(mptr->mutAttr), PTHREAD_PROCESS_SHARED);
    pthread_mutex_init(&(mptr->mutVal), &(mptr->mutAttr));
  }
}

void _SharedMutex_destroy(SharedMutex *mptr, void **privateMutexHandle) {
  *privateMutexHandle = NULL;
  if ( mptr != NULL ) {
    pthread_mutex_destroy(&(mptr->mutVal));
    pthread_mutexattr_destroy(&(mptr->mutAttr));
  }
}

int _SharedMutex_lock(SharedMutex *mptr, void **privateMutexHandle) {
  return pthread_mutex_lock(&(mptr->mutVal));
}

int _SharedMutex_unlock(SharedMutex *mptr, void **privateMutexHandle) {
  return pthread_mutex_unlock(&(mptr->mutVal));
}

// returns NULL if failed
HsPtr _store_alloc(const char *memBlockName, void **privateStoreHandle, size_t size) {
  int memFd = shm_open(memBlockName, O_CREAT | O_RDWR, S_IRWXU);
  if (memFd < 0) {
    return NULL;
  }
  int res = ftruncate(memFd, size);
  if (res != 0) {
    shm_unlink(memBlockName);
    return NULL;
  }
  HsPtr r = mmap( NULL
                , size
                , PROT_READ | PROT_WRITE
                , MAP_SHARED
                , memFd, 0);
  if (r == MAP_FAILED) {
    shm_unlink(memBlockName);
    return NULL;
  }
  return r;
}

// failures of system calls are ignored
void _store_free( const char *memBlockName, void **privateStoreHandle, HsPtr addr
                , size_t size, _Bool unlinkToo) {
  if(addr != 0){
    munmap(addr, size);
  }
  if(unlinkToo && memBlockName[0] != '\0') {
    shm_unlink(memBlockName);
  }
}
