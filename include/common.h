#ifndef __HSCOMMON_H__
#define __HSCOMMON_H__

#define SHARED_OBJECT_NAME_LENGTH 32

#ifndef HS_IMPORT_CONSTANTS_ONLY
#include <Rts.h>

/**
 * The input is an StgTSO* wrapped in a stable pointer.
 * The StgTSO object can be moved during GC, which can happen while we're
 * in a safe/interruptible foreign call.
 *
 * By performing this check, I can find out if there are any pending async
 * exceptions in the calling (and suspended) Haskell thread. I need this,
 * because on Windows there seem to be no way to interrupt / check the
 * interruption status in between `WaitForSingleObject` or alike using the
 * provided GHC machinery (CancelSynchronousIo).
 *
 * @param [in] tso a stable pointer to an StgTSO object.
 * @return whether there are any pending blocked exceptions on this Haskell
 * thread.
 */
bool has_blocked_exceptions(StgStablePtr tso);

typedef char SharedObjectName[SHARED_OBJECT_NAME_LENGTH];

void genSharedObjectName(char* const name);

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

#endif /* HS_IMPORT_CONSTANTS_ONLY */
#endif /* __HSCOMMON_H__ */
