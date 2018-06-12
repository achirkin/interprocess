# interprocess mvar-fail

Haskell + C code example with a bug that makes some processes to hang indefinitely on `pthread_cond_timedwait`.

#### Story

I tried to implement Haskell `Control.Concurrent.MVar` that resides in shared memory
and allows communicating between multiple independent processes/programs using POSIX functionality.
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
The bug happens usually after 1-5 iterations,
but depending on the moon and stars it can take a hundred iterations for the bug to occur.
Increasing the number of threads to 50 guarantees the quick bug occurrence.

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

Here is a full output example running with 1 master and 2 slaves with my comments on the right after "sharp" symbol
(all three processes hang visible in the process manager as `futex_wait_queue_me`).

```bash
[A] Started.
new - init...
new - finished!
[A] Done! Put lots of MVars for other threads                  # main process spawned all slaves
[A] Putting 1
[0] mvar_put 1 locked, isFull: 0
[0] mvar_put 1 copying
[0] mvar_put 1 finished
[A] Have put 1
[A] Putting 2
[0] mvar_put 2 locked, isFull: 1
[0] mvar_put 2 will wait 50, isFull 1, errno 0
[B] (2) Started.
[2] lookup - mapped memory, r = 0, errno = 0, size = 264
[2] lookup - success; errno: 0; name: /HsIPC.nGogGZICvRFsE02. # process [2] found the shared mvar
[2] 1010 Taking.
[2] mvar_take 1010 entering, errno 0
[2] mvar_take 1010 locked, isFull: 1, errno 0
[2] mvar_take 1010 copying
[2] mvar_take 1010 finished
[2] 1010 Taken. Putting.
[2] mvar_put 1010 locked, isFull: 0
[2] mvar_put 1010 copying
[2] mvar_put 1010 finished
[2] 1010 Have put.
[2] 1009 Taking.
[2] mvar_take 1009 entering, errno 0
[2] mvar_take 1009 locked, isFull: 1, errno 0
[2] mvar_take 1009 copying
[2] mvar_take 1009 finished
[2] 1009 Taken. Putting.
[2] mvar_put 1009 locked, isFull: 0
[2] mvar_put 1009 copying
[2] mvar_put 1009 finished
[2] 1009 Have put.
[2] 1008 Taking.
[2] mvar_take 1008 entering, errno 0
[2] mvar_take 1008 locked, isFull: 1, errno 0
[2] mvar_take 1008 copying
[2] mvar_take 1008 finished
[2] 1008 Taken. Putting.
[2] mvar_put 1008 locked, isFull: 0
[2] mvar_put 1008 copying
[2] mvar_put 1008 finished
[2] 1008 Have put.
[2] 1007 Taking.
[2] mvar_take 1007 entering, errno 0
[2] mvar_take 1007 locked, isFull: 1, errno 0
[2] mvar_take 1007 copying
[2] mvar_take 1007 finished
[2] 1007 Taken. Putting.
[2] mvar_put 1007 locked, isFull: 0
[2] mvar_put 1007 copying
[2] mvar_put 1007 finished
[2] 1007 Have put.
[2] 1006 Taking.
[2] mvar_take 1006 entering, errno 0
[2] mvar_take 1006 locked, isFull: 1, errno 0
[2] mvar_take 1006 copying
[2] mvar_take 1006 finished
[2] 1006 Taken. Putting.
[B] (1) Started.
[2] mvar_put 1006 locked, isFull: 0
[2] mvar_put 1006 copying
[2] mvar_put 1006 finished
[2] 1006 Have put.
[2] 1005 Taking.
[2] mvar_take 1005 entering, errno 0
[2] mvar_take 1005 locked, isFull: 1, errno 0
[2] mvar_take 1005 copying
[2] mvar_take 1005 finished
[2] 1005 Taken. Putting.
[2] mvar_put 1005 locked, isFull: 0
[2] mvar_put 1005 copying
[2] mvar_put 1005 finished
[2] 1005 Have put.
[1] lookup - mapped memory, r = 0, errno = 0, size = 264
[1] lookup - success; errno: 0; name: /HsIPC.nGogGZICvRFsE02.  # process [1] found the shared mvar
[1] 1010 Taking.
[2] 1004 Taking.
[2] mvar_take 1004 entering, errno 2
[2] mvar_take 1004 locked, isFull: 1, errno 2
[2] mvar_take 1004 copying
[2] mvar_take 1004 finished
[2] 1004 Taken. Putting.
[2] mvar_put 1004 locked, isFull: 0
[2] mvar_put 1004 copying
[2] mvar_put 1004 finished
[2] 1004 Have put.
[2] 1003 Taking.
[2] mvar_take 1003 entering, errno 2
[2] mvar_take 1003 locked, isFull: 1, errno 2
[2] mvar_take 1003 copying
[2] mvar_take 1003 finished
[2] 1003 Taken. Putting.
[2] mvar_put 1003 locked, isFull: 0
[2] mvar_put 1003 copying
[2] mvar_put 1003 finished
[2] 1003 Have put.
[2] 1002 Taking.
[2] mvar_take 1002 entering, errno 2
[2] mvar_take 1002 locked, isFull: 1, errno 2
[2] mvar_take 1002 copying
[2] mvar_take 1002 finished
[2] 1002 Taken. Putting.
[2] mvar_put 1002 locked, isFull: 0
[2] mvar_put 1002 copying
[2] mvar_put 1002 finished
[2] 1002 Have put.
[2] 1001 Taking.
[2] mvar_take 1001 entering, errno 2
[2] mvar_take 1001 locked, isFull: 1, errno 2
[2] mvar_take 1001 copying
[2] mvar_take 1001 finished
[1] mvar_take 1010 entering, errno 0
[2] 1001 Taken. Putting.
[1] mvar_take 1010 locked, isFull: 0, errno 0
[1] mvar_take 1010 will wait 50, isFull 0, errno 0             # the last message from the worker thread of slave process [1]
[0] mvar_put 2 wait done 50, isFull 0, r = 0, errno 0
[0] mvar_put 2 copying
[0] mvar_put 2 finished
[A] Have put 2
[A] Have put all vals. Waiting
Finished withNProcesses, waiting for procs to finish
[2] mvar_put 1001 locked, isFull: 1
[2] mvar_put 1001 will wait 50, isFull 1, errno 2
[2] mvar_put 1001 wait done 50, isFull 1, r = 110, errno 2
[2] mvar_put 1001 wait timeout 50, isFull 1
[2] mvar_put 1001 will wait 100, isFull 1, errno 2
[2] mvar_put 1001 wait done 100, isFull 1, r = 110, errno 2
[2] mvar_put 1001 wait timeout 100, isFull 1
[2] mvar_put 1001 will wait 200, isFull 1, errno 2
[0] destroy - starting...
[0] destroy... some users left 2
[0] destroy... finished                                        # the main thread finished here, two MVar users left
[2] mvar_put 1001 wait done 200, isFull 1, r = 110, errno 2
[2] mvar_put 1001 wait timeout 200, isFull 1
[2] mvar_put 1001 will wait 400, isFull 1, errno 2
[2] mvar_put 1001 wait done 400, isFull 1, r = 110, errno 2
[2] mvar_put 1001 wait timeout 400, isFull 1
[2] mvar_put 1001 will wait 800, isFull 1, errno 2
[2] Was waiting the worker thread for 1 sec
[1] Was waiting the worker thread for 1 sec
[2] mvar_put 1001 wait done 800, isFull 1, r = 110, errno 2
[2] mvar_put 1001 wait timeout 800, isFull 1
[2] mvar_put 1001 will wait 1600, isFull 1, errno 2
[1] Was waiting the worker thread for 2 sec
[2] Was waiting the worker thread for 2 sec
[2] mvar_put 1001 wait done 1600, isFull 1, r = 110, errno 2
[2] mvar_put 1001 wait timeout 1600, isFull 1
[2] mvar_put 1001 will wait 3200, isFull 1, errno 2
[2] mvar_put 1001 wait done 3200, isFull 1, r = 110, errno 2
[2] mvar_put 1001 wait timeout 3200, isFull 1
[2] mvar_put 1001 will wait 6400, isFull 1, errno 2            # process [2] keeps waking up and waiting forever
[2] Was waiting the worker thread for 4 sec                    # both slaves' main threads keep waiting their worker threads to finish
[1] Was waiting the worker thread for 4 sec                    # both slaves' main threads keep waiting their worker threads to finish
...
```
