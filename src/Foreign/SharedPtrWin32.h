#ifndef __HSSHAREDPTRWIN32_H__
#define __HSSHAREDPTRWIN32_H__
#include <windows.h>


typedef char StoreName[32];

typedef struct SharedMutex {
  StoreName mutexName;
} SharedMutex;

#endif /* __HSSHAREDPTRWIN32_H__ */
