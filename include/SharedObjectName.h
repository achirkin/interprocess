#ifndef __HSSHAREDOBJECTNAME_H__
#define __HSSHAREDOBJECTNAME_H__

#define SharedObjectNameLength 32
typedef char SharedObjectName[SharedObjectNameLength];
void genSharedObjectName(char * const name);

#endif /* __HSSHAREDOBJECTNAME_H__ */
