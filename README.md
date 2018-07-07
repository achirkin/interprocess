# interprocess

WIP: platform-independent interprocess communication.

The core feature is shared memory allocator implemented using POSIX mmap and Windows CreateFileMapping.
You can create as many allocators as you want (as your RAM can afford) and
concurrently malloc and free memory in different processes using them.

Features and TODO:

  * [x] `Foreign.SharedPtr` -- `malloc`, `realloc` and `free` in the shared memory region
        that can be accessed by multiple processes.
  * [x] Semaphores
  * [ ] Mutexes (not sure if need this)
  * [x] Mutable variables via `Storable` instance plus garbage collection.
  * [ ] Proper error messages
  * [ ] Debug, verbose mode

The idea of the library is to address GHC stop-the-world GC problem:

  1. Create several instances of your program in different isolated processes
     using e.g. `typed-process` library.
  2. Establish shared memory and semaphore usage via this program
  3. Garbage collection events in one process do not affect another one at all. Profit!
