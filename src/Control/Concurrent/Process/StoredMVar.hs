{-# LANGUAGE InterruptibleFFI    #-}
{-# LANGUAGE ScopedTypeVariables #-}
-----------------------------------------------------------------------------
-- |
--   This module is an adaptation of `Control.Concurrent.MVar` to an
--   interprocess communication (IPC).
--   The IPC setting implies a few changes to the interface.
--
--   1. `StoredMVar` resides in a shared memory region.
--
--   2. We use `Storable` instance to serialize and deserialize a value.
--
--   3. Point (2) implies the value is always fully evaluated before being stored.
--
--   4. Scheduling is done by OS, thus the module does not guarantee FIFO order.
--
--   5. Using `StoredMVar` is only safe if `Storable` instance for its content
--      is correct and `peek` does not throw exceptions.
--      If `peek` throws an exception inside `takeMVar` or `swapMVar`,
--      the original content of `StoredMVar` is not restored
--
-----------------------------------------------------------------------------
module Control.Concurrent.Process.StoredMVar
  ( StoredMVar (), mVarName
  , newEmptyMVar, newMVar, lookupMVar
  , takeMVar, putMVar, readMVar, swapMVar
  , tryTakeMVar, tryPutMVar, tryReadMVar, trySwapMVar
  , isEmptyMVar
  , withMVar, withMVarMasked
  , modifyMVar, modifyMVar_, modifyMVarMasked, modifyMVarMasked_
  ) where

import           Control.Exception
import           Control.Monad                     (when)
import           Data.Data                         (Typeable)
import           Foreign.C
import           Foreign.ForeignPtr
import           Foreign.Marshal.Alloc             (alloca)
import           Foreign.Marshal.Array             (advancePtr, allocaArray)
import           Foreign.Ptr
import           Foreign.SharedObjectName.Internal
import           Foreign.Storable

-- | Opaque implementation-dependent StoredMVar
data StoredMVarT

-- | An 'StoredMVar' is a synchronising variable, used
--   for communication between concurrent processes or threads.
--   It can be thought of as a a box, which may be empty or full.
--
--   @StoredMVar@ tries to mimic vanilla `MVar`, though it behaves quite differently.
--   It uses `Storable` instance to make the value accessible in different memory spaces.
--   Thus, the content of @StoredMVar@ is forced to be fully evaluated and serialized.
data StoredMVar a
  = StoredMVar !(SOName (StoredMVar a)) !(ForeignPtr StoredMVarT)
  deriving (Eq, Typeable)


-- | Create a 'StoredMVar' which is initially empty.
newEmptyMVar :: forall a . Storable a => IO (StoredMVar a)
newEmptyMVar = mask_ $ do
    mvar <- checkNullPointer "newEmptyMVar"
          . c'mvar_new . fromIntegral $ sizeOf (undefined :: a)
    n <- newEmptySOName
    unsafeWithSOName n $ c'mvar_name mvar
    StoredMVar n <$> newForeignPtr p'mvar_destroy mvar

-- | Create a 'StoredMVar' which is initially empty.
newMVar :: Storable a => a -> IO (StoredMVar a)
newMVar value = do
    x <- newEmptyMVar
    putMVar x value
    return x


-- | Find a `StoredMVar` created in another process ot thread by its reference.
lookupMVar :: Storable a => SOName (StoredMVar a) ->  IO (StoredMVar a)
lookupMVar n = mask_ $ do
    mvar <- unsafeWithSOName n $ checkNullPointer "lookupMVar". c'mvar_lookup
    StoredMVar n <$> newForeignPtr p'mvar_destroy mvar

-- | Get a global reference to the `StoredMVar`.
--   Send this reference to another process to lookup this `StoredMVar` and
--   start interprocess communication.
mVarName :: StoredMVar a -> SOName (StoredMVar a)
mVarName (StoredMVar r _) = r
{-# INLINE mVarName #-}

-- | Check whether a given 'StoredMVar' is empty.
--
--   Notice that the boolean value returned  is just a snapshot of
--   the state of the MVar. By the time you get to react on its result,
--   the MVar may have been filled (or emptied) - so be extremely
--   careful when using this operation.  Use 'tryTakeMVar' instead if possible.
isEmptyMVar :: StoredMVar a -> IO Bool
isEmptyMVar (StoredMVar _ fp) = withForeignPtr fp $ fmap (0 /=) . c'mvar_isempty
{-# INLINE isEmptyMVar #-}


-- | Return the contents of the 'StoredMVar'.  If the 'StoredMVar' is currently
--   empty, 'takeMVar' will wait until it is full.  After a 'takeMVar',
--   the 'StoredMVar' is left empty.
--
--
--   * 'takeMVar' is single-wakeup.  That is, if there are multiple
--     processes blocked in 'takeMVar', and the 'StoredMVar' becomes full,
--     only one thread will be woken up.
--
--   * The library makes no guarantees about the order in which processes
--     are woken up. This is all up to implementation-dependent OS scheduling.
--
takeMVar :: Storable a => StoredMVar a -> IO a
takeMVar (StoredMVar _ fp) = mask_ $ withForeignPtr fp $ \p -> alloca $ \lp -> do
    r <- c'mvar_take p lp
    if r == 0
    then peek lp
    else throwErrno $ "takeMVar failed with code " ++ show r
{-# INLINE takeMVar #-}


-- | Atomically read the contents of an 'StoredMVar'.  If the 'StoredMVar' is
--   currently empty, 'readMVar' will wait until its full.
--   'readMVar' is guaranteed to receive the next 'putMVar'.
--
--  'readMVar' is multiple-wakeup, so when multiple readers are
--    blocked on an 'StoredMVar', all of them are woken up at the same time.
--
readMVar :: Storable a => StoredMVar a -> IO a
readMVar (StoredMVar _ fp) = mask_ $ withForeignPtr fp $ \p -> alloca $ \lp -> do
    r <- c'mvar_read p lp
    if r == 0
    then peek lp
    else throwErrno $ "readMVar failed with code " ++ show r
{-# INLINE readMVar #-}


-- | Atomically take a value from an 'StoredMVar', put a new value into the 'StoredMVar' and
--   return the value taken.
swapMVar :: Storable a => StoredMVar a -> a -> IO a
swapMVar (StoredMVar _ fp) x
  = mask_ $ withForeignPtr fp $ \p -> allocaArray 2 $ \inp -> do
    let outp = advancePtr inp 1
    poke inp x
    r <- c'mvar_swap p inp outp
    if r == 0
    then peek outp
    else throwErrno $ "takeMVar failed with code " ++ show r
{-# INLINE swapMVar #-}


-- | Put a value into an 'StoredMVar'.  If the 'StoredMVar' is currently full,
--   'putMVar' will wait until it becomes empty.
--
--
--   * 'putMVar' is single-wakeup.  That is, if there are multiple threads
--     or processes blocked in 'putMVar', and the 'StoredMVar' becomes empty,
--     only one thread will be woken up.
--
--   * The library makes no guarantees about the order in which processes
--     are woken up. This is all up to implementation-dependent OS scheduling.
--
putMVar :: Storable a => StoredMVar a -> a -> IO ()
putMVar (StoredMVar _ fp) x = mask_ $ withForeignPtr fp $ \p -> alloca $ \lp -> do
    poke lp x
    r <- c'mvar_put p lp
    when (r /= 0) $ throwErrno $ "putMVar failed with code " ++ show r
{-# NOINLINE putMVar #-}

-- | A non-blocking version of 'takeMVar'.  The 'tryTakeMVar' function
--   returns immediately, with 'Nothing' if the 'StoredMVar' was empty, or
--   @'Just' a@ if the 'StoredMVar' was full with contents @a@.
--   After 'tryTakeMVar', the 'StoredMVar' is left empty.
tryTakeMVar :: Storable a => StoredMVar a -> IO (Maybe a)
tryTakeMVar (StoredMVar _ fp) = mask_ $ withForeignPtr fp $ \p -> alloca $ \lp -> do
    r <- c'mvar_trytake p lp
    if r == 0 then Just <$> peek lp
              else return Nothing
{-# INLINE tryTakeMVar #-}

-- | A non-blocking version of 'readMVar'.
--   The 'tryReadMVar' function
--   returns immediately, with 'Nothing' if the 'StoredMVar' was empty, or
--   @'Just' a@ if the 'StoredMVar' was full with contents @a@.
--
tryReadMVar :: Storable a => StoredMVar a -> IO (Maybe a)
tryReadMVar (StoredMVar _ fp) = mask_ $ withForeignPtr fp $ \p -> alloca $ \lp -> do
    r <- c'mvar_tryread p lp
    if r == 0 then Just <$> peek lp
              else return Nothing
{-# INLINE tryReadMVar #-}

-- | A non-blocking version of 'putMVar'.
--   The 'tryPutMVar' function
--   attempts to put the value @a@ into the 'StoredMVar', returning 'True' if
--   it was successful, or 'False' otherwise.
tryPutMVar  :: Storable a => StoredMVar a -> a -> IO Bool
tryPutMVar (StoredMVar _ fp) x = mask_ $ withForeignPtr fp $ \p -> alloca $ \lp -> do
    poke lp x
    r <- c'mvar_tryput p lp
    return $ r == 0
{-# INLINE tryPutMVar #-}

-- | A non-blocking version of 'swapMVar'.
--   Atomically attempt take a value from an 'StoredMVar', put a new value into the 'StoredMVar' and
--   return the value taken (thus, leave the `StoredMVar` full).
--   Return @Nothing@ if the `StoredMVar` was empty (and leave it empty).
trySwapMVar :: Storable a => StoredMVar a -> a -> IO (Maybe a)
trySwapMVar (StoredMVar _ fp) x
  = mask_ $ withForeignPtr fp $ \p -> allocaArray 2 $ \inp -> do
    let outp = advancePtr inp 1
    poke inp x
    r <- c'mvar_tryswap p inp outp
    if r == 0
    then Just <$> peek outp
    else return Nothing
{-# INLINE trySwapMVar #-}

checkNullPointer :: String -> IO (Ptr a) -> IO (Ptr a)
checkNullPointer s k = do
  p <- k
  if p == nullPtr
  then throwErrno ("StoredMVar." ++ s ++ ": FFI returned NULL pointer.")
  else return p
{-# INLINE checkNullPointer #-}


foreign import ccall unsafe "mvar_new"
  c'mvar_new :: CSize -> IO (Ptr StoredMVarT)

foreign import ccall unsafe "mvar_lookup"
  c'mvar_lookup :: CString -> IO (Ptr StoredMVarT)

foreign import ccall unsafe "&mvar_destroy"
  p'mvar_destroy :: FunPtr (Ptr StoredMVarT -> IO ())

foreign import ccall unsafe "mvar_name"
  c'mvar_name :: Ptr StoredMVarT -> CString -> IO ()

-- | Waits a lot and should be interruptible
foreign import ccall interruptible "mvar_take"
  c'mvar_take :: Ptr StoredMVarT -> Ptr a -> IO CInt
-- | Waits a bit and may be unsafe
foreign import ccall unsafe "mvar_trytake"
  c'mvar_trytake :: Ptr StoredMVarT -> Ptr a -> IO CInt
-- | Waits a lot and should be interruptible
foreign import ccall interruptible "mvar_put"
  c'mvar_put :: Ptr StoredMVarT -> Ptr a -> IO CInt
-- | Waits a bit and may be unsafe
foreign import ccall unsafe "mvar_tryput"
  c'mvar_tryput :: Ptr StoredMVarT -> Ptr a -> IO CInt
-- | Waits a lot and should be interruptible
foreign import ccall interruptible "mvar_read"
  c'mvar_read :: Ptr StoredMVarT -> Ptr a -> IO CInt
-- | Does not wait and can be unsafe
foreign import ccall unsafe "mvar_tryread"
  c'mvar_tryread :: Ptr StoredMVarT -> Ptr a -> IO CInt
-- | Waits a lot and should be interruptible
foreign import ccall interruptible "mvar_swap"
  c'mvar_swap :: Ptr StoredMVarT -> Ptr a -> Ptr a -> IO CInt
-- | Waits a bit and may be unsafe
foreign import ccall unsafe "mvar_tryswap"
  c'mvar_tryswap :: Ptr StoredMVarT -> Ptr a -> Ptr a -> IO CInt
-- | Does not wait and can be unsafe
foreign import ccall unsafe "mvar_isempty"
  c'mvar_isempty :: Ptr StoredMVarT -> IO CInt



-- | 'withMVar' is an exception-safe wrapper for operating on the contents
--   of an 'StoredMVar'.  This operation is exception-safe: it will replace the
--   original contents of the 'StoredMVar' if an exception is raised (see
--   "Control.Exception").  However, it is only atomic if there are no
--  other producers for this 'StoredMVar'.
withMVar :: Storable a => StoredMVar a -> (a -> IO b) -> IO b
withMVar m io = mask $ \restore -> do
    a <- takeMVar m
    b <- restore (io a) `onException` putMVar m a
    putMVar m a
    return b
{-# INLINE withMVar #-}


-- | Like 'withMVar', but the @IO@ action in the second argument is executed
--   with asynchronous exceptions masked.
withMVarMasked :: Storable a => StoredMVar a -> (a -> IO b) -> IO b
withMVarMasked m io = mask_ $ do
    a <- takeMVar m
    b <- io a `onException` putMVar m a
    putMVar m a
    return b
{-# INLINE withMVarMasked #-}


-- | An exception-safe wrapper for modifying the contents of an 'StoredMVar'.
--   Like 'withMVar', 'modifyMVar' will replace the original contents of
--   the 'StoredMVar' if an exception is raised during the operation.  This
--   function is only atomic if there are no other producers for this
--   'StoredMVar'.
modifyMVar_ :: Storable a => StoredMVar a -> (a -> IO a) -> IO ()
modifyMVar_ m io = mask $ \restore -> do
    a  <- takeMVar m
    a' <- restore (io a) `onException` putMVar m a
    putMVar m a'
{-# INLINE modifyMVar_ #-}


-- | A slight variation on 'modifyMVar_' that allows a value to be
--   returned (@b@) in addition to the modified value of the 'StoredMVar'.
modifyMVar :: Storable a => StoredMVar a -> (a -> IO (a,b)) -> IO b
modifyMVar m io = mask $ \restore -> do
    a      <- takeMVar m
    (a',b) <- restore (io a >>= evaluate) `onException` putMVar m a
    putMVar m a'
    return b
{-# INLINE modifyMVar #-}


-- | Like 'modifyMVar_', but the @IO@ action in the second argument is executed with
--   asynchronous exceptions masked.
modifyMVarMasked_ :: Storable a => StoredMVar a -> (a -> IO a) -> IO ()
modifyMVarMasked_ m io = mask_ $ do
    a  <- takeMVar m
    a' <- io a `onException` putMVar m a
    putMVar m a'
{-# INLINE modifyMVarMasked_ #-}


-- | Like 'modifyMVar', but the @IO@ action in the second argument is executed with
--   asynchronous exceptions masked.
modifyMVarMasked :: Storable a => StoredMVar a -> (a -> IO (a,b)) -> IO b
modifyMVarMasked m io = mask_ $ do
    a      <- takeMVar m
    (a',b) <- (io a >>= evaluate) `onException` putMVar m a
    putMVar m a'
    return b
{-# INLINE modifyMVarMasked #-}
