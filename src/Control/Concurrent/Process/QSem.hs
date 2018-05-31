{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
-- | Simple interprocess quantity semaphores
--
--   Based on POSIX or Win32 C semaphores
module Control.Concurrent.Process.QSem
  ( QSem, newQSem, lookupQSem, waitQSem, signalQSem
  , QSemRef, getQSemRef
  , hPutQSemRef, hGetQSemRef
  ) where

import           Control.Monad             (when)
import           Foreign.C.Error
import           Foreign.C.String
import           Foreign.C.Types
import           Foreign.ForeignPtr
import           Foreign.ForeignPtr.Unsafe
import           Foreign.Ptr
import           Foreign.Storable
import           System.IO

#define HS_IMPORT_CONSTANTS_ONLY
#include "SharedObjectName.h"

-- | Opaque implementation-dependent semaphore
data QSemT

-- | Reference to QSem; can be sent to other processes.
newtype QSemRef = QSemRef (ForeignPtr CChar)
  deriving (Show)

-- | 'QSem' is a quantity semaphore in which the resource is aqcuired
--   and released in units of one.
data QSem = QSem !QSemRef !(ForeignPtr QSemT)


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
    nPtr <- mallocForeignPtrBytes SharedObjectNameLength
    withForeignPtr nPtr $ c'qsem_name qsem
    QSem (QSemRef nPtr) <$> newForeignPtr p'qsem_close qsem

-- | Lookup QSem by its name in the global namespace.
--   Use this function to init several entangled semaphores in different processes.
--
--   This function throws an exception if no `QSem` with this name exist,
--   or if an underlying platform-dependent function fails.
lookupQSem :: QSemRef -> IO QSem
lookupQSem q@(QSemRef n) = do
  qsem <- withForeignPtr n $ checkNullPointer "lookupQSem" . c'qsem_lookup
  QSem q <$> newForeignPtr p'qsem_close qsem

-- | Get a global reference to the semaphore.
--   Send this reference to another process to lookup this semaphore and
--   start interprocess communication.
getQSemRef :: QSem -> QSemRef
getQSemRef (QSem r _) = r

-- | Wait for a unit to become available
--
--   This function throws an exception
--   if an underlying platform-dependent function fails.
waitQSem :: QSem -> IO ()
waitQSem (QSem _ p) = withForeignPtr p $ checkZeroReturn "waitQSem" . c'qsem_wait

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

-- | Check first if two CString point to the same memory location.
--   Otherwise, compare them using C @strcmp@ function.
cmpCStrings :: CString -> CString -> Ordering
cmpCStrings a b
  | a == b = EQ
  | otherwise = c'strcmp a b `compare` 0


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

foreign import ccall unsafe "qsem_name"
  c'qsem_name :: Ptr QSemT -> CString -> IO ()

foreign import ccall unsafe "strcmp"
  c'strcmp :: CString -> CString -> CInt

foreign import ccall unsafe "memcpy"
  c'memcpy :: Ptr a -> Ptr b -> CInt -> IO ()

instance Eq QSemRef where
  (QSemRef a) == (QSemRef b)
    = cmpCStrings (unsafeForeignPtrToPtr a) (unsafeForeignPtrToPtr b) == EQ

instance Ord QSemRef where
  compare (QSemRef a) (QSemRef b)
    = cmpCStrings (unsafeForeignPtrToPtr a) (unsafeForeignPtrToPtr b)

instance Storable QSemRef where
  sizeOf _ = SharedObjectNameLength
  alignment _ = 8
  poke p (QSemRef qp) = withForeignPtr qp
      $ \q -> c'memcpy p q
                 SharedObjectNameLength
  peek p = do
    qp <- mallocForeignPtrBytes SharedObjectNameLength
    withForeignPtr qp
      $ \q -> c'memcpy q p
                 SharedObjectNameLength
    return $ QSemRef qp


hPutQSemRef :: Handle -> QSemRef -> IO ()
hPutQSemRef h a@(QSemRef q) = withForeignPtr q $ \p -> do
  hPutBuf h p (sizeOf a)
  hFlush h

hGetQSemRef :: Handle -> IO (Maybe QSemRef)
hGetQSemRef h = do
  let n = sizeOf (undefined :: QSemRef)
  q <- mallocForeignPtrBytes n
  n' <- withForeignPtr q $ \p -> hGetBuf h p n
  return $
    if n' < n
    then Nothing
    else Just (QSemRef q)
