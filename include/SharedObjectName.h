#ifndef __HSSHAREDOBJECTNAME_H__
#define __HSSHAREDOBJECTNAME_H__

#define SharedObjectNameLength 32
#ifndef HS_IMPORT_CONSTANTS_ONLY
typedef char SharedObjectName[SharedObjectNameLength];
void genSharedObjectName(char * const name);
#endif

#endif /* __HSSHAREDOBJECTNAME_H__ */
