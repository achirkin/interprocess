{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE InterruptibleFFI #-}
module Control.Concurrent.Process.StoredMVar
  ( StoredMVar (), mVarName
  , newEmptyMVar, newMVar, lookupMVar
  , takeMVar, putMVar
  -- , readMVar
  -- , tryTakeMVar, tryPutMVar, tryReadMVar
  ) where

import           Data.Data                         (Typeable)
import           Foreign.C.Error
import           Foreign.C.String
import           Foreign.C.Types
import           Foreign.ForeignPtr
import           Foreign.Ptr
import           Foreign.SharedObjectName.Internal
import           Foreign.Storable
import           Foreign.Marshal.Alloc (alloca)
import Control.Exception (evaluate, mask_) -- , uninterruptibleMask_, mask_)

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
newEmptyMVar = eio $ do
    mvar <- eio . checkNullPointer "newEmptyMVar"
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
lookupMVar n = do
    mvar <- unsafeWithSOName n $ checkNullPointer "lookupMVar". c'mvar_lookup
    StoredMVar n <$> newForeignPtr p'mvar_destroy mvar

-- | Get a global reference to the `StoredMVar`.
--   Send this reference to another process to lookup this `StoredMVar` and
--   start interprocess communication.
mVarName :: StoredMVar a -> SOName (StoredMVar a)
mVarName (StoredMVar r _) = r

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
takeMVar (StoredMVar _ fp) = withForeignPtr fp $ \p -> alloca $ \lp -> do
    r <- c'mvar_take p lp
    if r == 0
    then peek lp
    else do
      putStrLn $ "takeMVar failed with code " ++ show r
      throwErrno $ "takeMVar failed with code " ++ show r


-- -- | Atomically read the contents of an 'StoredMVar'.  If the 'StoredMVar' is
-- --   currently empty, 'readMVar' will wait until its full.
-- --   'readMVar' is guaranteed to receive the next 'putMVar'.
-- --
-- --  'readMVar' is multiple-wakeup, so when multiple readers are
-- --    blocked on an 'MVar', all of them are woken up at the same time.
-- --
-- readMVar :: Storable a => StoredMVar a -> IO a
-- readMVar (StoredMVar _ fp) = eio $ withForeignPtr fp $ \p -> alloca $ \lp -> do
--     c'mvar_read p lp
--     peek lp
-- {-# NOINLINE readMVar #-}


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
putMVar (StoredMVar _ fp) x = mask_ $ eio $ withForeignPtr fp $ \p -> alloca $ \lp -> do
    poke lp x
    r <- c'mvar_put p lp
    if r == 0
    then return ()
    else do
      putStrLn $ "putMVar failed with code " ++ show r
      throwErrno $ "putMVar failed with code " ++ show r
{-# NOINLINE putMVar #-}

-- -- | A non-blocking version of 'takeMVar'.  The 'tryTakeMVar' function
-- --   returns immediately, with 'Nothing' if the 'StoredMVar' was empty, or
-- --   @'Just' a@ if the 'StoredMVar' was full with contents @a@.
-- --   After 'tryTakeMVar', the 'StoredMVar' is left empty.
-- tryTakeMVar :: Storable a => StoredMVar a -> IO (Maybe a)
-- tryTakeMVar (StoredMVar _ fp) = eio $ withForeignPtr fp $ \p -> alloca $ \lp -> do
--     r <- c'mvar_trytake p lp
--     if r == 0 then Just <$> peek lp
--               else return Nothing
--
--
-- -- | A non-blocking version of 'readMVar'.
-- --   The 'tryReadMVar' function
-- --   returns immediately, with 'Nothing' if the 'StoredMVar' was empty, or
-- --   @'Just' a@ if the 'StoredMVar' was full with contents @a@.
-- --
-- tryReadMVar :: Storable a => StoredMVar a -> IO (Maybe a)
-- tryReadMVar (StoredMVar _ fp) = eio $ withForeignPtr fp $ \p -> alloca $ \lp -> do
--     r <- c'mvar_tryread p lp
--     if r == 0 then Just <$> peek lp
--               else return Nothing
--
-- -- | A non-blocking version of 'putMVar'.
-- --   The 'tryPutMVar' function
-- --   attempts to put the value @a@ into the 'StoredMVar', returning 'True' if
-- --   it was successful, or 'False' otherwise.
-- tryPutMVar  :: Storable a => StoredMVar a -> a -> IO Bool
-- tryPutMVar (StoredMVar _ fp) x = eio $ withForeignPtr fp $ \p -> alloca $ \lp -> do
--     poke lp x
--     r <- c'mvar_tryput p lp
--     return $ r == 0

checkNullPointer :: String -> IO (Ptr a) -> IO (Ptr a)
checkNullPointer s k = do
  p <- k
  if p == nullPtr
  then throwErrno (s ++ " returned NULL pointer.")
  else return p

eio :: IO a -> IO a
eio m = m >>= evaluate

foreign import ccall unsafe "mvar_new"
  c'mvar_new :: CSize -> IO (Ptr StoredMVarT)

foreign import ccall unsafe "mvar_lookup"
  c'mvar_lookup :: CString -> IO (Ptr StoredMVarT)

foreign import ccall unsafe "&mvar_destroy"
  p'mvar_destroy :: FunPtr (Ptr StoredMVarT -> IO ())

foreign import ccall unsafe "mvar_name"
  c'mvar_name :: Ptr StoredMVarT -> CString -> IO ()


foreign import ccall interruptible "mvar_take"
  c'mvar_take :: Ptr StoredMVarT -> Ptr a -> IO CInt
-- foreign import ccall unsafe "mvar_trytake"
--   c'mvar_trytake :: Ptr StoredMVarT -> Ptr a -> IO CInt
foreign import ccall interruptible "mvar_put"
  c'mvar_put :: Ptr StoredMVarT -> Ptr a -> IO CInt
-- foreign import ccall unsafe "mvar_tryput"
--   c'mvar_tryput :: Ptr StoredMVarT -> Ptr a -> IO CInt
-- foreign import ccall unsafe "mvar_read"
--   c'mvar_read :: Ptr StoredMVarT -> Ptr a -> IO ()
-- foreign import ccall unsafe "mvar_tryread"
--   c'mvar_tryread :: Ptr StoredMVarT -> Ptr a -> IO CInt
