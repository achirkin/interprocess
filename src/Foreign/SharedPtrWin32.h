#ifndef __HSSHAREDPTRWIN32_H__
#define __HSSHAREDPTRWIN32_H__
#include "SharedObjectName.h"

typedef struct SharedMutex {
  SharedObjectName mutexName;
} SharedMutex;

#endif /* __HSSHAREDPTRWIN32_H__ */
