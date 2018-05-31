/* Modified version of "Buddy system" from
     "The Art of Computer Programming" by D.Knuth, vol 1, ch 2.5.

   Shared memory functions align all allocated memory to the system memory page size,
   which is usually 4KB.
   This has two implications for the algorithm:
     1. Minimum storage block size is equal to 4096
     2. All allocated memory is aligned to min(2^k, 4096) bytes, where k is
        minimum power such that allocsize < 2^k

 */
#include "SharedPtr.h"

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <string.h>


/* Default store size is at least the page size, which usualy is equal to 4KB.
 */
#define DEFAULT_STORE_SIZE_FACTOR 12
#define DEFAULT_STORE_SIZE        4096
/*
  SharedPtrs define the store id and offset in the store;
  storeId == 0 is reserved for SharedAllocData;
  AvailableStorage must be not bigger than DEFAULT_STORE_SIZE to be addressable.

  MIN_ALLOC_FACTOR: k where 2^k = 2*sizeof(HsWord)+1
  MAX_STORES:       (WORD_SIZE_IN_BITS - DEFAULT_STORE_SIZE_FACTOR + 1)
 */
#if SIZEOF_HSWORD == 4
#define MIN_ALLOC_FACTOR 4
#elif SIZEOF_HSWORD == 8
#define MIN_ALLOC_FACTOR 5
#else
#error Unknown WORD size architecture. Cannot proceed anymore.
#endif

typedef struct SharedMutex SharedMutex;


/* Determine necessary memory block size based on the requested size;
   the block size must be:
     1. power of two;
     2. >= 2*sizeof(HsWord) + 1
 */
HsWord8 allocFactor(size_t size){
  HsWord8 r = MIN_ALLOC_FACTOR;
  size_t x = size >> MIN_ALLOC_FACTOR;
  while(x > 0){
    r++;
    x = x >> 1;
  }
  return r;
}

/* Get storeId from the sharedPtr.
   StoreId is defined by the largest bit of the pointer.
 */
HsWord8 getStoreId(const SharedPtr ptr){
  if (ptr < DEFAULT_STORE_SIZE ) {
    return 0;
  }
  HsWord8 r = DEFAULT_STORE_SIZE_FACTOR;
  SharedPtr x = ptr >> (DEFAULT_STORE_SIZE_FACTOR+1);
  while(x > 0){
    r++;
    x = x >> 1;
  }
  return r;
}

/* Tag byte is to the left from allocation block,
   to preserve data alignment, it has a negative position (-1).
   The only exception is that tag for the first block is kept in the last
   allocated memory addresed in a corresponding storage.
 */
#define STORAGE_TAG_BYTE_PTR(storePtr, storeSize, ptr) \
   ( (HsWord8*) \
     (  ( ( (((HsWord)ptr) - ((HsWord)storePtr)) + (((HsWord)storeSize) - ((HsWord)1)) ) \
        % ((HsWord)storeSize) \
        ) + ((HsWord)storePtr) \
     ) \
   ) \

/* This linked list contains pointers to available chunks of allocated memory
   and an id of used store.
 */
typedef struct ListNode {
  SharedPtr linkB;   // previous free block
  SharedPtr linkF;   // next free block
} ListNode;


/*
  Store a table of linked lists, one per each memory size.

 */
typedef ListNode AvailableStorage[WORD_SIZE_IN_BITS];


typedef struct SharedAllocData {
  // Table of linked lists, one for each power-of-two memory size.
  // Must go first, to be within first DEFAULT_STORE_SIZE of the shared memory block
  AvailableStorage availStorage;
  // Names of mmap-allocated storage chunks,
  // storeNames[0] is the name of storage used to keep SharedAllocData itself
  StoreName        storeNames[WORD_SIZE_IN_BITS];
  // How many allocators point to this data? only the last one should unlink an allocated memory.
  HsWord           usersN;
  // Current largest allocated storage chunk
  HsWord8          largestStoreId;
  // Protect shared data acces
  SharedMutex      mutex;
} SharedAllocData;

/* Allocator keeps part of its data in private memory, part in shared memory.
   .storeAddrs[0] === .sharedAllocData
 */
typedef struct SharedAllocator{
  union {
    SharedAllocData *sharedAllocData;
    HsPtr            storeAddrs[WORD_SIZE_IN_BITS];
  };
  // ids of stores sorted by their mmapped addressed ASC
  HsWord8          storeIdsSorted[WORD_SIZE_IN_BITS];
  // Total number of initialized stores, including the main one.
  HsWord8          storeN;
  // Windows-use only: HANDLE to mutex
  void            *mutexPrivateHandle;
  // Windows-use only: HANDLEs to stores
  void            *storePrivateHandles[WORD_SIZE_IN_BITS];
} SharedAllocator;


int _unique_seed = 0;
void _gen_unique(char *ptr, int added_seed){
  if(_unique_seed == 0){
    srand(time(NULL));
  }
  sprintf(ptr
    , "/HsSharedPtr-%08x%08x", rand() ^ 0x19a628f6 ^ added_seed, _unique_seed++);
}

// returns NULL if failed
SharedAllocData *_SharedAllocData_init(void **privateStoreHandle, void **mutexPrivateHandle){
  StoreName stName = { 0 };
  _gen_unique(stName, 1968293 ^ (int)((HsWord64)&stName));
  SharedAllocData *sdataPtr
    = (SharedAllocData*)_store_alloc(stName, privateStoreHandle, sizeof(SharedAllocData));
  if (sdataPtr == NULL) {
    return NULL;
  }
  memset(sdataPtr, 0, sizeof(SharedAllocData));
  SharedPtr curNode;
  for(HsInt i = 0; i < WORD_SIZE_IN_BITS; i++) {
    curNode = ((SharedPtr) &(sdataPtr->availStorage[i])) - ((SharedPtr) sdataPtr);
    sdataPtr->availStorage[i]
      = (struct ListNode)
        { .linkB = curNode
        , .linkF = curNode
        };
  }
  memcpy(sdataPtr->storeNames[0], stName, sizeof(StoreName));
  sdataPtr->largestStoreId = 0;
  _SharedMutex_init(&(sdataPtr->mutex), mutexPrivateHandle, 1);
  return sdataPtr;
}

void _SharedAllocData_destroy( SharedAllocData *sdataPtr
                             , void **privateStoreHandle
                             , void **mutexPrivateHandle
                             , const _Bool isLastUser
                             ){
  _SharedMutex_destroy(isLastUser ? &(sdataPtr->mutex) : NULL, mutexPrivateHandle);
  StoreName myName = {0};
  memcpy(myName, sdataPtr->storeNames[0], sizeof(StoreName));
  _store_free( myName
             , privateStoreHandle
             , sdataPtr
             , sizeof(SharedAllocData)
             , isLastUser
             );
}

SharedAllocator *shared_createAllocator(){
  SharedAllocator *aptr = (SharedAllocator*)malloc(sizeof(SharedAllocator));
  if ( aptr == NULL ) {
    return NULL;
  }
  memset(aptr, 0, sizeof(SharedAllocator));
  SharedAllocData *sdataPtr
    = _SharedAllocData_init(&(aptr->storePrivateHandles[0]), &(aptr->mutexPrivateHandle));
  if ( sdataPtr == NULL ) {
    free(aptr);
    return NULL;
  }
  aptr->sharedAllocData = sdataPtr;
  aptr->storeN = 1;
  aptr->sharedAllocData->usersN = 1;
  return aptr;
}

SharedAllocator *shared_lookupAllocator(const char *storeName){
  SharedAllocator *aptr = (SharedAllocator*)malloc(sizeof(SharedAllocator));
  if ( aptr == NULL ) {
    return NULL;
  }
  memset(aptr, 0, sizeof(SharedAllocator));
  SharedAllocData *sdataPtr = (SharedAllocData*)
    _store_alloc(storeName, &(aptr->storePrivateHandles[0]), sizeof(SharedAllocData));
  if ( sdataPtr == NULL ) {
    free(aptr);
    return NULL;
  }
  aptr->sharedAllocData = sdataPtr;
  aptr->storeN = 1;
  sdataPtr->usersN++;
  _SharedMutex_init(&(sdataPtr->mutex), &(aptr->mutexPrivateHandle), 0);
  return aptr;
}

void shared_destroyAllocator(SharedAllocator *aptr) {
  SharedAllocData *sdataPtr = aptr->sharedAllocData;
  int mlock = _SharedMutex_lock(&(sdataPtr->mutex), &(aptr->mutexPrivateHandle));
  int remainingUsers = --(sdataPtr->usersN);
  if(mlock == 0){
    _SharedMutex_unlock(&(sdataPtr->mutex), &(aptr->mutexPrivateHandle));
  }

  // release all stores except the main one
  for (HsWord8 storeId = DEFAULT_STORE_SIZE_FACTOR;
       storeId <= sdataPtr->largestStoreId;
       storeId++) {
    _store_free( sdataPtr->storeNames[storeId]
               , &(aptr->storePrivateHandles[storeId])
               , aptr->storeAddrs[storeId]
               , ((size_t)1) << storeId
               , remainingUsers == 0
               );
  }
  _SharedAllocData_destroy( sdataPtr
                          , &(aptr->storePrivateHandles[0])
                          , &(aptr->mutexPrivateHandle)
                          , remainingUsers == 0
                          );
  free(aptr);
}

char *shared_getStoreName(SharedAllocator *aptr){
  return aptr->sharedAllocData->storeNames[0];
}


SharedPtr shared_ptrToShPtr(SharedAllocator *aptr, void *ptr) {
  HsWord8 i = aptr->storeN - 1;
  HsWord8 storeId = aptr->storeIdsSorted[i];
  while (i > 0 && aptr->storeAddrs[storeId] > ptr) {
    storeId = aptr->storeIdsSorted[--i];
  }

  SharedPtr base = (SharedPtr)ptr - (SharedPtr)(aptr->storeAddrs[storeId]);
  if (storeId == 0) {
    assert(base < DEFAULT_STORE_SIZE);
    return base;
  } else {
    assert(storeId >= DEFAULT_STORE_SIZE_FACTOR && storeId < WORD_SIZE_IN_BITS);
    assert(base < (((SharedPtr)1) << storeId));
    return base | (((SharedPtr)1) << storeId);
  }
}

int _shared_initStore(SharedAllocator * const aptr, const HsWord8 storeId);

// Must return a valid pointer if the system is intact.
// This function returning NULL with valid allocator means implementation is broken.
// If the allocator is erroneous, behavior is undefined.
void *shared_shPtrToPtr(SharedAllocator *aptr, SharedPtr ptr){
  HsWord8 storeId = getStoreId(ptr);
  SharedPtr storeAddr = (SharedPtr)(aptr->storeAddrs[storeId]);
  if(storeAddr == 0){
    int initSuccess = _shared_initStore(aptr, storeId);
    assert(initSuccess == 0);
    storeAddr = (SharedPtr)(aptr->storeAddrs[storeId]);
  }
  if (storeId == 0) {
    assert(ptr < DEFAULT_STORE_SIZE);
    return (void*)(storeAddr + ptr);
  } else {
    assert(storeId >= DEFAULT_STORE_SIZE_FACTOR && storeId < WORD_SIZE_IN_BITS);
    return (void*)(storeAddr + (ptr & ~(((SharedPtr)1)<<storeId)));
  }
}

// Requires: storeId > 0
// returns:
//   0 on success
//   1 otherwise
int _shared_initStore(SharedAllocator * const aptr, const HsWord8 storeId){
  // do nothing if store already exists
  if ( aptr->storeAddrs[storeId] != 0 || storeId == 0) {
    return 0;
  }
  assert(storeId >= DEFAULT_STORE_SIZE_FACTOR && storeId < WORD_SIZE_IN_BITS);
  SharedAllocData *sdataPtr = aptr->sharedAllocData;
  HsPtr storePtr;
  size_t storeSize = ((size_t)1) << storeId;
  // if store already exists, just add it to local context
  if (sdataPtr->storeNames[storeId][0] != '\0'){
    storePtr
      = _store_alloc( sdataPtr->storeNames[storeId], &(aptr->storePrivateHandles[storeId]), storeSize );
    if (storePtr == NULL) {
      return 1;
    }
  } else {
    // generate a new unique name for the store
    _gen_unique( sdataPtr->storeNames[storeId], (int)(((size_t)&aptr) ^ storeSize) );
    // allocate store
    storePtr
      = _store_alloc( sdataPtr->storeNames[storeId], &(aptr->storePrivateHandles[storeId]), storeSize );
    if (storePtr == NULL) {
      return 1;
    }
    // init first free block in the newly created store
    ListNode *firstNode = &(sdataPtr->availStorage[storeId]);
    ListNode *secondNode = (ListNode*)shared_shPtrToPtr(aptr, firstNode->linkF);
    // last byte of the storage stands for (storePtr-1)
    *((HsWord8*)storePtr + storeSize - 1)
      = storeId | 0x80; // first bit is flag, others are free storage size (==storeSize)
    ListNode *newNode = (ListNode*)storePtr;
    SharedPtr newNodePtr = (SharedPtr)storeSize;
    newNode->linkF = firstNode->linkF;
    newNode->linkB = secondNode->linkB;
    // insert an availBlock node into the list
    firstNode->linkF = newNodePtr;
    secondNode->linkB = newNodePtr;
    // insert update shared data
    if (storeId > sdataPtr->largestStoreId) {
      sdataPtr->largestStoreId = storeId;
    }
  }
  aptr->storeAddrs[storeId] = storePtr;
  // bubble-sort-insert new store into the sorted pool
  HsWord8 i = aptr->storeN;
  while (i > 0 &&  storePtr
                 < (aptr->storeAddrs[aptr->storeIdsSorted[i-1]]) ) {
    aptr->storeIdsSorted[i] = aptr->storeIdsSorted[i-1];
    i--;
  }
  aptr->storeIdsSorted[i] = storeId;
  aptr->storeN++;
  return 0;
}



static inline void *_shared_malloc(SharedAllocator *aptr, size_t size) {
  HsWord8 storeId;
  HsWord8 allocSizeBit = allocFactor(size);
  HsWord8 availSizeBit = allocSizeBit;
  ListNode* nptr = &(aptr->sharedAllocData->availStorage[availSizeBit]);
  // if SharedPtr value is less than DEFAULT_STORE_SIZE, it points to the default store,
  // which implies the list is empty.
  // {while list is empty and we cannot init store of this size}
  while (  nptr->linkF < DEFAULT_STORE_SIZE
        && (  availSizeBit < DEFAULT_STORE_SIZE_FACTOR
           || aptr->sharedAllocData->storeNames[availSizeBit][0] != '\0'
           )
        ) {
    availSizeBit++;
    nptr = &(aptr->sharedAllocData->availStorage[availSizeBit]);
  }
  // By now, either we found a free block, or we can create one
  if ( nptr->linkF < DEFAULT_STORE_SIZE ) {
    assert(availSizeBit >= DEFAULT_STORE_SIZE_FACTOR);
    if (_shared_initStore(aptr,availSizeBit) != 0) {
      return NULL;
    }
    nptr = &(aptr->sharedAllocData->availStorage[availSizeBit]);
  }
  storeId = getStoreId(nptr->linkF);
  assert(storeId >= DEFAULT_STORE_SIZE_FACTOR && storeId < WORD_SIZE_IN_BITS);
  size_t storeSize = ((size_t)1) << storeId;
  // By now, nptr list is not empty, and all lists allocSizeBit..availSizeBit-1 are empty.
  ListNode *allocNodePtr, *secondNodePtr;
  HsWord8* storePtr = aptr->storeAddrs[storeId];
  if (storePtr == 0) {
    if (_shared_initStore(aptr, storeId) != 0) {
      return NULL;
    } else {
      storePtr = aptr->storeAddrs[storeId];
    }
  }
  // Now, nptr points to the beginning of non-empty list of free nodes of required size.
  // Allocate a node!
  allocNodePtr = (ListNode*)shared_shPtrToPtr(aptr, nptr->linkF);

  // remove node from the list
  secondNodePtr = (ListNode*)shared_shPtrToPtr(aptr, allocNodePtr->linkF);
  secondNodePtr->linkB = allocNodePtr->linkB;
  nptr->linkF = allocNodePtr->linkF;

  // Check if we need to split the allocated region into buddies
  ListNode *buddyNodePtr;
  SharedPtr buddySharedPtr;
  while ( availSizeBit > allocSizeBit ) {

    availSizeBit--;
    nptr = &(aptr->sharedAllocData->availStorage[availSizeBit]);
    // add buddy to corresponding list
    buddyNodePtr = (ListNode*)((SharedPtr)allocNodePtr + (((SharedPtr)1) << availSizeBit));
    buddySharedPtr = (SharedPtr)(((size_t)buddyNodePtr - (size_t)storePtr) | storeSize);
    *buddyNodePtr = (struct ListNode)
      { .linkB = nptr->linkB
      , .linkF = nptr->linkF
      };
    nptr->linkF = buddySharedPtr;
    nptr->linkB = buddySharedPtr;
    *(((HsWord8*)buddyNodePtr) - 1) = availSizeBit | 0x80;
  }
  // set flag to occupied
  *STORAGE_TAG_BYTE_PTR(storePtr, storeSize, allocNodePtr) = allocSizeBit;
  // return it!
  return (void*)allocNodePtr;
}

static inline void _shared_free(SharedAllocator *aptr, void *ptr) {
  ListNode *myPtr = (ListNode*)ptr;
  SharedPtr mySharedPtr = shared_ptrToShPtr(aptr, ptr);
  HsWord8 storeId = getStoreId(mySharedPtr);
  assert(storeId >= DEFAULT_STORE_SIZE_FACTOR && storeId < WORD_SIZE_IN_BITS);
  size_t storeSize = ((size_t)1) << storeId; // guaranteed: storeId != 0
  HsWord8 *storePtr = aptr->storeAddrs[storeId];
  HsWord8 nodeSizeBit = *STORAGE_TAG_BYTE_PTR(storePtr, storeSize, ptr);
  assert(nodeSizeBit >= MIN_ALLOC_FACTOR && nodeSizeBit <= storeId);
  // join buddies if needed
  ListNode *buddyPtr
      = (ListNode*)( (((SharedPtr)myPtr - (SharedPtr)storePtr) ^ (((SharedPtr)1) << nodeSizeBit))
                    + (SharedPtr)storePtr );

  while (  nodeSizeBit < storeId
        && *STORAGE_TAG_BYTE_PTR(storePtr, storeSize, buddyPtr) == (nodeSizeBit | 0x80)
        ) {
    // remove buddy from corresponding list;
    ListNode *tmpNode;
    tmpNode = (ListNode *)shared_shPtrToPtr(aptr, buddyPtr->linkF);
    tmpNode->linkB = buddyPtr->linkB;
    tmpNode = (ListNode *)shared_shPtrToPtr(aptr, buddyPtr->linkB);
    tmpNode->linkF = buddyPtr->linkF;
    // take the left one further
    if(buddyPtr < myPtr) {
      myPtr    = buddyPtr;
      mySharedPtr = ((SharedPtr)myPtr - (SharedPtr)storePtr) | (SharedPtr)storeSize;
    }
    nodeSizeBit++;
    buddyPtr
      = (ListNode*)( (((SharedPtr)myPtr - (SharedPtr)storePtr) ^ (((SharedPtr)1) << nodeSizeBit))
                    + (SharedPtr)storePtr );
  }
  *STORAGE_TAG_BYTE_PTR(storePtr, storeSize, myPtr) = nodeSizeBit | 0x80;
  // Now, there are no buddies to join; add the node to the pool
  ListNode *firstNodePtr, *secondNodePtr;
  firstNodePtr = &(aptr->sharedAllocData->availStorage[nodeSizeBit]);
  secondNodePtr = (ListNode*)shared_shPtrToPtr(aptr, firstNodePtr->linkF);
  myPtr->linkF = firstNodePtr->linkF;
  myPtr->linkB = secondNodePtr->linkB;
  firstNodePtr->linkF = mySharedPtr;
  secondNodePtr->linkB = mySharedPtr;
}


static inline void *_shared_realloc(SharedAllocator *aptr, void *ptr, size_t size) {
  SharedPtr nodeSharedPtr = shared_ptrToShPtr(aptr, ptr);
  HsWord8 storeId = getStoreId(nodeSharedPtr);
  size_t storeSize = ((size_t)1) << storeId; // guaranteed: storeId != 0
  HsWord8 *storePtr = aptr->storeAddrs[storeId];
  HsWord8 nodeSizeBit = *STORAGE_TAG_BYTE_PTR(storePtr, storeSize, ptr);
  HsWord8 allocSizeBit = allocFactor(size);

  void *buddyPtr;
  // free extra memory if not needed anymore
  while ( nodeSizeBit > allocSizeBit) {
    nodeSizeBit--;
    buddyPtr
       = (void*)( ( ( (SharedPtr)ptr - (SharedPtr)storePtr )
                    ^ (((SharedPtr)1) << nodeSizeBit)
                  ) + (SharedPtr)storePtr );
    if ( buddyPtr < ptr ) {
      nodeSizeBit++;
      break;
    }
    *STORAGE_TAG_BYTE_PTR(storePtr, storeSize, buddyPtr) = nodeSizeBit;
    *STORAGE_TAG_BYTE_PTR(storePtr, storeSize, ptr) = nodeSizeBit;
    _shared_free(aptr, buddyPtr);
  }
  // use buddy memory if available
  if (allocSizeBit <= storeId) {
    ListNode *buddyPrev, *buddyNext;
    while ( nodeSizeBit < allocSizeBit ) {
      buddyPtr
         = (void*)( ( ( (SharedPtr)ptr - (SharedPtr)storePtr )
                      ^ (((SharedPtr)1) << nodeSizeBit)
                    ) + (SharedPtr)storePtr );
      if ( buddyPtr < ptr || (*STORAGE_TAG_BYTE_PTR(storePtr, storeSize, buddyPtr) != (nodeSizeBit | 0x80)) ) {
        break;
      }
      // remove node from the list
      buddyPrev = (ListNode*)shared_shPtrToPtr(aptr, ((ListNode*)buddyPtr)->linkB);
      buddyNext = (ListNode*)shared_shPtrToPtr(aptr, ((ListNode*)buddyPtr)->linkF);
      buddyPrev->linkF = ((ListNode*)buddyPtr)->linkF;
      buddyNext->linkB = ((ListNode*)buddyPtr)->linkB;
      nodeSizeBit++;
      *STORAGE_TAG_BYTE_PTR(storePtr, storeSize, ptr) = nodeSizeBit;
    }
  }
  // return the same pointer if the size matches
  if ( nodeSizeBit == allocSizeBit ) {
    return ptr;
  }
  // copy data because cheaper options failed
  void *rptr = _shared_malloc(aptr, size);
  if ( rptr == NULL ) {
    return NULL;
  }
  size_t toCopy = (((size_t)1) << nodeSizeBit) - 1;
  if (toCopy > size) {
    toCopy = size;
  }
  memcpy( rptr, ptr, toCopy);
  _shared_free(aptr, ptr);

  return rptr;
}




void *shared_malloc(SharedAllocator *aptr, size_t size){
  int mlock = _SharedMutex_lock(&(aptr->sharedAllocData->mutex), &(aptr->mutexPrivateHandle));
  if ( mlock != 0 ) {
    return NULL;
  }
  void* r = _shared_malloc(aptr, size);
  mlock = _SharedMutex_unlock(&(aptr->sharedAllocData->mutex), &(aptr->mutexPrivateHandle));
  if ( mlock != 0 ) {
    return NULL;
  }
  return r;
}

void *shared_realloc(SharedAllocator *aptr, void *ptr, size_t size) {
  int mlock = _SharedMutex_lock(&(aptr->sharedAllocData->mutex), &(aptr->mutexPrivateHandle));
  if ( mlock != 0 ) {
    return NULL;
  }
  void* r = _shared_realloc(aptr, ptr, size);
  mlock = _SharedMutex_unlock(&(aptr->sharedAllocData->mutex), &(aptr->mutexPrivateHandle));
  if ( mlock != 0 ) {
    return NULL;
  }
  return r;
}

void  shared_free(SharedAllocator *aptr, void *ptr) {
  int mlock = _SharedMutex_lock(&(aptr->sharedAllocData->mutex), &(aptr->mutexPrivateHandle));
  _shared_free(aptr, ptr);
  if ( mlock == 0 ) {
    _SharedMutex_unlock(&(aptr->sharedAllocData->mutex), &(aptr->mutexPrivateHandle));
  }
}
