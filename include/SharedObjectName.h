#ifndef __HSSHAREDOBJECTNAME_H__
#define __HSSHAREDOBJECTNAME_H__

#define SharedObjectNameLength 32
#if defined(WIN32) || defined(_WIN32) || defined(__WIN32) || defined(mingw32_HOST_OS)
#include <windows.h>
typedef TCHAR SharedObjectName[SharedObjectNameLength];
void genSharedObjectName(LPTSTR const name);
#else
typedef char SharedObjectName[SharedObjectNameLength];
void genSharedObjectName(char * name);
#endif

#endif /* __HSSHAREDOBJECTNAME_H__ */
