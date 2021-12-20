# interprocess
[![Hackage](https://img.shields.io/hackage/v/interprocess.svg)](https://hackage.haskell.org/package/interprocess)
[![Build Status](https://app.travis-ci.com/achirkin/interprocess.svg)](http://app.travis-ci.com/achirkin/interprocess)

Platform-independent interprocess communication.

This project provides a shared memory allocator and some synchronization primitives
for Win32 and POSIX systems.


#### SharedObjectName

`Foreign.SharedObjectName.SOName` is a globally unique name that can be used to lookup
shared objects across processes.
Internally, it is a `ForeignPtr` to a C-string with a fixed length.
The library provides `Eq`, `Show`, and `Storable` instances and helper functions
to transfer `SOName` via pipes or by any other means.

#### SharedPtr

`Foreign.SharedPtr` provides a custom shared memory `Allocator` and a bunch of functions
similar to vanilla `Ptr`.
Memory allocation is implemented using POSIX mmap and Windows CreateFileMapping APIs.
You can create as many allocators as you want (as your RAM can afford) and
concurrently malloc and free memory in different processes using them.
All functions of that module are wrappers on C functions from `Foreign.SharedPtr.C`.
The latter can be used to pass the allocation functions as pointers
(those C functions do not need Haskell runtime, thus can be used in unsafe callbacks).

#### Control.Concurrent.Process

`Control.Concurrent.Process.*` provide a few synchronization primitives trying
to mirror the interface of `Control.Concurrent.*` modules for the IPC case.
The behavior is slightly different due to IPC limitations.
Internally, these use semaphore, mutexes, condition variables, and events
from Win32/POSIX in a platform-dependent way.

### TODO

  * [x] `Foreign.SharedPtr` -- `malloc`, `realloc` and `free` in the shared memory region
        that can be accessed by multiple processes.
  * [x] Semaphores
  * [x] Mutable variables (`MVar`-like) via `Storable` instance.
  * [ ] `Control.Concurrent.Chan`-like channels
  * [ ] More tests
  * [ ] Ensure Win32 waiting on events interruptible
  * [ ] Benchmarks


### Think about it

There is an untested idea to address GHC stop-the-world GC problem:

  1. Create several instances of your program in different isolated processes
     using e.g. `typed-process` library.
  2. Establish shared memory and semaphore usage via this library
  3. Garbage collection events in one process do not affect another one at all.
     Profit!

The question is if the cost of IPC synchronization is lower than the added
cost of collecting garbage in all parallel threads.
