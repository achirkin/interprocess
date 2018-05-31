#ifndef __HSSHAREDPTRPOSIX_H__
#define __HSSHAREDPTRPOSIX_H__
#include "SharedObjectName.h"
#include <pthread.h>

typedef struct SharedMutex {
  pthread_mutex_t     mutVal;
  pthread_mutexattr_t mutAttr;
} SharedMutex;

#endif /* __HSSHAREDPTRPOSIX_H__ */
