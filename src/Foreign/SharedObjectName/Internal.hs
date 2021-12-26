{-# LANGUAGE CPP             #-}
{-# LANGUAGE RoleAnnotations #-}
-- | Exposed internals of `Foreign.SharedObjectName`.
--
module Foreign.SharedObjectName.Internal
  ( SOName (..), hPutSOName, hGetSOName, unsafeWithSOName
  , genSOName, newEmptySOName
  ) where

import           Foreign.C.String
import           Foreign.C.Types
import           Foreign.ForeignPtr
import           Foreign.ForeignPtr.Unsafe
import           Foreign.Ptr
import           Foreign.Storable
import           System.IO
import           System.IO.Unsafe
import           Text.Read

#define HS_IMPORT_CONSTANTS_ONLY
#include "MachDeps.h"
#include "common.h"

-- | Reference to a shared object; can be sent to other processes.
newtype SOName a = SOName (ForeignPtr CChar)
type role SOName phantom

instance Show (SOName a) where
    showsPrec d (SOName a)
      = showParen (d >= 10) $ showString "SOName " . showsPrec 10 getstr
      where
        getstr = unsafePerformIO $ withForeignPtr a peekCAString
        {-# NOINLINE getstr #-}

instance Read (SOName a) where
    readPrec = parens $ prec 10 $ do
        Ident "SOName" <- lexP
        s <- step readPrec
        return $ putstr s
      where
        writeStr [] n     ptr
          = pokeElemOff ptr n 0 -- put end of string character
        writeStr (c:cs) n ptr
          = pokeElemOff ptr n (castCharToCChar c) >> writeStr cs (n+1) ptr
        putstr s = unsafePerformIO $ do
          n <- newEmptySOName
          unsafeWithSOName n $ writeStr s 0
          return n
        {-# NOINLINE putstr #-}

instance Eq (SOName a) where
    (SOName a) == (SOName b)
      = cmpCStrings (unsafeForeignPtrToPtr a) (unsafeForeignPtrToPtr b) == EQ

instance Ord (SOName a) where
    compare (SOName a) (SOName b)
      = cmpCStrings (unsafeForeignPtrToPtr a) (unsafeForeignPtrToPtr b)

instance Storable (SOName a) where
    sizeOf _ = SHARED_OBJECT_NAME_LENGTH
    alignment _ = SIZEOF_HSWORD
    poke p (SOName qp) = withForeignPtr qp
        $ \q -> c_memcpy p q SHARED_OBJECT_NAME_LENGTH
    peek p = do
      qp <- mallocForeignPtrBytes SHARED_OBJECT_NAME_LENGTH
      withForeignPtr qp
        $ \q -> c_memcpy q p SHARED_OBJECT_NAME_LENGTH
      return $ SOName qp

-- | Write a shared object name into somwhere referenced by a handle.
--   Useful for sending references to other processes via pipes.
hPutSOName :: Handle -> SOName a -> IO ()
hPutSOName h (SOName q)
    = withForeignPtr q $ flip (hPutBuf h) SHARED_OBJECT_NAME_LENGTH

-- | Read a shared object name from somwhere referenced by a handle.
--   Returns @Nothing@ if @hGetBuf@ gets less than @SHARED_OBJECT_NAME_LENGTH@ bytes.
--   Useful for sending references to other processes via pipes.
hGetSOName :: Handle -> IO (Maybe (SOName a))
hGetSOName h = do
    let n = SHARED_OBJECT_NAME_LENGTH
    q <- mallocForeignPtrBytes n
    n' <- withForeignPtr q $ \p -> hGetBuf h p n
    return $
      if n' < n
      then Nothing
      else Just (SOName q)

-- | Generate a new unique shared object name.
genSOName :: IO (SOName a)
genSOName = do
    fp <- mallocForeignPtrBytes SHARED_OBJECT_NAME_LENGTH
    withForeignPtr fp c_genSharedObjectName
    return $ SOName fp

-- | Allocate a new shared object name.
newEmptySOName :: IO (SOName a)
newEmptySOName = SOName <$> mallocForeignPtrBytes SHARED_OBJECT_NAME_LENGTH


-- | Use a pointer to a C string to pass to some low-level (e.g. foreign) functions.
--   `SOName` is asserted immutable, so do not modify it!
unsafeWithSOName :: SOName a -> (CString -> IO b) -> IO b
unsafeWithSOName (SOName fp) = withForeignPtr fp

-- | Check first if two CString point to the same memory location.
--   Otherwise, compare them using C @strcmp@ function.
cmpCStrings :: CString -> CString -> Ordering
cmpCStrings a b
    | a == b = EQ
    | otherwise = c_strcmp a b `compare` 0

foreign import ccall unsafe "strcmp"
    c_strcmp :: CString -> CString -> CInt

foreign import ccall unsafe "memcpy"
    c_memcpy :: Ptr a -> Ptr b -> CInt -> IO ()

foreign import ccall unsafe "genSharedObjectName"
    c_genSharedObjectName :: CString -> IO ()
