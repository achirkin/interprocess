#ifndef __HSCOMMON_H__
#define __HSCOMMON_H__

#ifdef INTERPROCESS_DEBUG
#include <stdio.h>
#define INTERPROCESS_LOG_DEBUG(...)       \
  do {                                    \
    printf("%s:%d ", __FILE__, __LINE__); \
    printf(__VA_ARGS__);                  \
  } while (0)
#else
#define INTERPROCESS_LOG_DEBUG(...) (void)0
#endif

#endif /* __HSCOMMON_H__ */
