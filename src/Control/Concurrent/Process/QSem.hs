{-# OPTIONS_GHC -funbox-strict-fields #-}
-- | Simple interprocess quantity semaphores
--
--   Based on POSIX or Win32 C semaphores
module Control.Concurrent.Process.QSem
  ( QSem, newQSem, lookupQSem, waitQSem, tryWaitQSem, signalQSem, qSemName
  ) where

import           Control.Monad                     (when)
import           Foreign.C.Error
import           Foreign.C.String
import           Foreign.C.Types
import           Foreign.ForeignPtr
import           Foreign.Ptr
import           Foreign.SharedObjectName.Internal


-- | Opaque implementation-dependent semaphore
data QSemT

-- | 'QSem' is a quantity semaphore in which the resource is aqcuired
--   and released in units of one.
data QSem = QSem !(SOName QSem) !(ForeignPtr QSemT)

-- | Build a new 'QSem' with a supplied initial quantity.
--   The initial quantity must be at least 0.
--
--   This function throws an exception
--   if an underlying platform-dependent function fails.
newQSem :: Int -> IO QSem
newQSem initial
  | initial < 0 = fail "newQSem: Initial quantity must be non-negative"
  | otherwise   = do
    qsem <- checkNullPointer "newQSem" $ c'qsem_new initial
    n <- newEmptySOName
    unsafeWithSOName n $ c'qsem_name qsem
    QSem n <$> newForeignPtr p'qsem_close qsem

-- | Lookup QSem by its name in the global namespace.
--   Use this function to init several entangled semaphores in different processes.
--
--   This function throws an exception if no `QSem` with this name exist,
--   or if an underlying platform-dependent function fails.
lookupQSem :: SOName QSem -> IO QSem
lookupQSem n = do
  qsem <- unsafeWithSOName n $ checkNullPointer "lookupQSem" . c'qsem_lookup
  QSem n <$> newForeignPtr p'qsem_close qsem

-- | Get a global reference to the semaphore.
--   Send this reference to another process to lookup this semaphore and
--   start interprocess communication.
qSemName :: QSem -> SOName QSem
qSemName (QSem r _) = r

-- | Wait for a unit to become available
--
--   This function throws an exception
--   if an underlying platform-dependent function fails.
waitQSem :: QSem -> IO ()
waitQSem (QSem _ p) = withForeignPtr p $ checkZeroReturn "waitQSem" . c'qsem_wait

-- | Try to take a unit of the `QSem`.
--
--   This function does not wait, in fact. Sorry for naming.
--
--   Returns:
--
--     * @True@ if successfully took a unit of `QSem` (it is decremented)
--     * @False@ if number of available units is less than @1@  (it is not decremented)
--
--   This function does not throw an exception.
tryWaitQSem :: QSem -> IO Bool
tryWaitQSem (QSem _ p) = withForeignPtr p $ fmap (0 ==) . c'qsem_trywait


-- | Signal that a unit of the 'QSem' is available
--
--   This function throws an exception
--   if an underlying platform-dependent function fails.
signalQSem :: QSem -> IO ()
signalQSem (QSem _ p) = withForeignPtr p $ checkZeroReturn "signalQSem" . c'qsem_signal


checkNullPointer :: String -> IO (Ptr a) -> IO (Ptr a)
checkNullPointer s k = do
  p <- k
  if p == nullPtr
  then throwErrno (s ++ " returned NULL pointer.")
  else return p

checkZeroReturn :: String -> IO CInt -> IO ()
checkZeroReturn s k = do
  p <- k
  when (p /= 0) $
    throwErrno (s ++ " returned non-zero result.")


foreign import ccall unsafe "qsem_new"
  c'qsem_new :: Int -> IO (Ptr QSemT)

foreign import ccall unsafe "qsem_lookup"
  c'qsem_lookup :: CString -> IO (Ptr QSemT)

foreign import ccall unsafe "&qsem_close"
  p'qsem_close :: FunPtr (Ptr QSemT -> IO ())

foreign import ccall unsafe "qsem_signal"
  c'qsem_signal :: Ptr QSemT -> IO CInt

foreign import ccall unsafe "qsem_wait"
  c'qsem_wait :: Ptr QSemT -> IO CInt

foreign import ccall unsafe "qsem_trywait"
  c'qsem_trywait :: Ptr QSemT -> IO CInt

foreign import ccall unsafe "qsem_name"
  c'qsem_name :: Ptr QSemT -> CString -> IO ()
