#ifndef __HSSHAREDOBJECTNAME_H__
#define __HSSHAREDOBJECTNAME_H__

#define SHARED_OBJECT_NAME_LENGTH 32
#ifndef HS_IMPORT_CONSTANTS_ONLY
typedef char SharedObjectName[SHARED_OBJECT_NAME_LENGTH];
void genSharedObjectName(char * const name);
#endif

#endif /* __HSSHAREDOBJECTNAME_H__ */
