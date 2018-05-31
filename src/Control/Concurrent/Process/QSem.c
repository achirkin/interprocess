#include "SharedObjectName.h"
#include "HsFFI.h"
#include <stdlib.h>

typedef struct QSem QSem;

QSem *qsem_new(HsInt count);
QSem *qsem_lookup(const char *name);
void  qsem_close(QSem *qsem);
int   qsem_signal(QSem *qsem);
int   qsem_wait(QSem *qsem);
void  qsem_name(QSem *qsem, char * const name);



#if defined(WIN32) || defined(_WIN32) || defined(__WIN32) || defined(mingw32_HOST_OS)
#include <windows.h>




#else
#include <fcntl.h>           /* For O_* constants */
#include <semaphore.h>
#include <string.h>
#include "HsFFI.h"

#define GuardNameSuffix "X"

typedef struct QSem {
  sem_t            *mainSem;
  sem_t            *guardSem;
  /*  Second semaphore is used to count number of
      extra users.
      That is, the quantity of a guardSem is (N-1) where N is number of processes
      currently having the semaphore initialized.
      When executing qsem_close, sem_trywait result defines if the semaphore
      should be unlinked or not.
   */
  SharedObjectName  mainName;
} QSem;

QSem *qsem_new(HsInt count) {
  QSem *r = malloc(sizeof(QSem));
  if (r == NULL) {
    return NULL;
  }
  genSharedObjectName(r->mainName);

  sem_t *mPtr = sem_open (r->mainName, O_CREAT | O_EXCL, 0600, count);
  if (mPtr == SEM_FAILED) {
    free(r);
    return NULL;
  }
  SharedObjectName guardName = {0};
  strcpy (guardName, r->mainName);
  strcat (guardName, GuardNameSuffix);
  sem_t *cPtr = sem_open (guardName, O_CREAT | O_EXCL, 0600, 0);
  if (mPtr == SEM_FAILED) {
    free(r);
    if (sem_close(mPtr) == 0){
      sem_unlink(r->mainName);
    }
    return NULL;
  }
  r->mainSem = mPtr;
  r->guardSem = cPtr;
  return r;
}

QSem *qsem_lookup(const char *name) {
  QSem *r = malloc(sizeof(QSem));
  if (r == NULL) {
    return NULL;
  }
  sem_t *mPtr = sem_open (name, 0);
  if (mPtr == SEM_FAILED) {
    free(r);
    return NULL;
  }
  SharedObjectName guardName = {0};
  strcpy (guardName, name);
  strcat (guardName, GuardNameSuffix);
  sem_t *cPtr = sem_open (guardName, 0);
  if (mPtr == SEM_FAILED || sem_post(cPtr) != 0) {
    free(r);
    sem_close(mPtr); // don't unlink semaphore in case it is used somewhere else.
    return NULL;
  }
  r->mainSem = mPtr;
  r->guardSem = cPtr;
  strcpy(r->mainName, name);
  return r;
}

void qsem_close(QSem *qsem) {
  sem_close(qsem->mainSem);
  if ( sem_trywait(qsem->guardSem) != 0 ) {
    sem_unlink(qsem->mainName);
    sem_close(qsem->guardSem);
    SharedObjectName guardName = {0};
    strcpy (guardName, qsem->mainName);
    strcat (guardName, GuardNameSuffix);
    sem_unlink(guardName);
  } else {
    sem_close(qsem->guardSem);
  }
  free(qsem);
}

int qsem_signal(QSem *qsem) {
  return sem_post(qsem->mainSem);
}

int qsem_wait(QSem *qsem) {
  return sem_wait(qsem->mainSem);
}

void qsem_name(QSem *qsem, char * const name) {
  strcpy(name, qsem->mainName);
}


#endif
