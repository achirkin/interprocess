# interprocess mvar-fail

Haskell + C code example with a bug that makes some processes to hang indefinitely on `pthread_cond_timedwait`.

#### Story

I tried to implement Haskell `Control.Concurrent.MVar` that resides in shared memory
and communicating between allows multiple independent processes/programs using POSIX functionality.
`MVar` is a mutable box, that can be empty or full; `putMVar` waits the box to be empty and copies data to it;
`takeMVar` waits the box to be full and copies data from it.
At some point, I got a lot of unpredictable deadlocks and tried to simplify the code to pinpoint the troubling part.
At the current stage, I removed all code that is not used to replicate the problem and added a lot of verbose debugging printfs.

#### Implementation

`StoredMVar.c` is responsible for synchronization:

  * `mvar_take` and `mvar_put` in C code directly correspond to `putMVar` and `takeMVar` in Haskell bindings.
  * There are two C structures: `MVar` is a process-local reference, `MVarState` is a global shared state of the mutable variable.
  * I use one mutex and two condition variable to control the state of `MVar` and signal its users.
  * All locks in the program are timed (`pthread_cond_timedwait` and `pthread_mutex_timedlock`). Must be not blockable forever, right?
 
 Haskell code is responsible for the `main` program entrance:
  
  * The first program starts in master mode and gets a single command line argument `N` -- number of slaves to starts
  * Every other process is started with first command line argument `slave` given by the master process
  * The master starts `N` processes from `N` independent `forkOS`ed threads.
  * The master (`runA`) (`[0]` or `[A]` in console output) calls `putMVar` `N` times.
  * Each slave (`runB`) calls `takeMVar` + `putMVar` 10 times and then `takeMVar` one time, such that in the end `MVar` is empty.
  * Each slave runs its code in a `forkOS`ed thread; its main thread waits for the result in a vanilla `MVar`
     (if the code runs in the main thread, the bug occurs less often).
 
 #### Replicating the problem
 
 I replicate the program reliebly on a few linux machines using the following script:
 ```bash
 while true; do reset && clear && stack build && stack exec wait-mvar -- 2; done
 ```
 The bug happens usually after 1-5 iterations. Increasing the number of threads to 50 guarantees the bug occurence.
 
 You will know that the bug has happened if the program does not finish.
 The output will tell that some processes are waiting in `putMVar` (correctly waking up from time to time),
 and at least one process is stuck in `pthread_cond_timedwait` trying to `takeMVar`.
 The latter does not try to wake up, causing the deadlock. It looks something like this:
 ```
 ...
 [i] mvar_take 1010 entering, errno 0
 [i] mvar_take 1010 locked, isFull: 0, errno 0
 [i] mvar_take 1010 will wait 50, isFull 0, errno 0
 ...
 ```
 Which indicates the process `[i]` never goes past line [302](https://github.com/achirkin/interprocess/blob/mvar-fail/src/StoredMVar.c#L302).
 
 Tested on a few versions of Ubuntu x64 (17.10+)
