{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
module Foreign.SharedPtr
  ( SharedPtr (), toSharedPtr, fromSharedPtr
  , AllocStoreName (..), Allocator
  , createAllocator, lookupAllocator, destroyAllocator
  , withNewAllocator, withAllocator, allocStoreName
  , malloc, mallocBytes, realloc, free
  ) where

import           Control.Exception   (bracket)
import           Data.Data           (Data)
import           Foreign.C.Error
import           Foreign.C.String
import           Foreign.Ptr
import           Foreign.SharedPtr.C
import           Foreign.Storable
import           GHC.Generics        (Generic)
import           System.IO.Unsafe    (unsafePerformIO)

-- | Global name of the store used by Allocator.
--   Use this name to `lookupAllocator` in another process.
newtype AllocStoreName = AllocStoreName { unAllocStoreName :: String }
  deriving (Eq, Ord, Show, Data, Generic)

-- | Make a portable shared pointer out of a regular pointer.
--   The result can be transfered to another process and re-created using
--   the shared `Allocator`.
toSharedPtr :: Allocator -> Ptr a -> SharedPtr a
toSharedPtr = c'shared_ptrToShPtr

-- | Reconstruct a regular pointer from a portable shared pointer.
--   Returns @NULL@ if shared pointer or allocator are not valid.
fromSharedPtr :: Allocator -> SharedPtr a -> Ptr a
fromSharedPtr = c'shared_shPtrToPtr

-- | Create a new `Allocator`.
createAllocator :: IO Allocator
createAllocator = checkNullPointer "SharedPtr.createAllocator"
                  c'shared_createAllocator
{-# INLINE createAllocator #-}


-- | Lookup a `Allocator` by its name.
--   Use this to share one allocator between multiple processes.
lookupAllocator :: AllocStoreName -> IO Allocator
lookupAllocator = checkNullPointer "SharedPtr.lookupAllocator"
                . flip withCString c'shared_lookupAllocator . unAllocStoreName
{-# INLINE lookupAllocator #-}

-- | Destroy allocator instance.
--   Note: memory is fully unlinked and released only after
--         the last allocator sharing the memory is destroyed.
destroyAllocator :: Allocator -> IO ()
destroyAllocator = c'shared_destroyAllocator
{-# INLINE destroyAllocator #-}

withNewAllocator :: (Allocator -> IO a) -> IO a
withNewAllocator = bracket createAllocator destroyAllocator
{-# INLINE withNewAllocator #-}

withAllocator :: AllocStoreName -> (Allocator -> IO a) -> IO a
withAllocator s = bracket (lookupAllocator s) destroyAllocator
{-# INLINE withAllocator #-}

allocStoreName :: Allocator -> AllocStoreName
allocStoreName
  = AllocStoreName . unsafePerformIO . peekCString . c'shared_getStoreName
{-# NOINLINE allocStoreName #-}


malloc :: Storable a => Allocator -> IO (Ptr a)
malloc a = go undefined
  where
    go :: Storable b => b -> IO (Ptr b)
    go x = mallocBytes a (sizeOf x)

mallocBytes :: Allocator -> Int -> IO (Ptr a)
mallocBytes a = checkNullPointer "SharedPtr.malloc"
              . c'shared_malloc a . fromIntegral

realloc :: Allocator -> Ptr a -> Int -> IO (Ptr a)
realloc a p = checkNullPointer "SharedPtr.realloc"
            . c'shared_realloc a p . fromIntegral

free :: Allocator -> Ptr a -> IO ()
free = c'shared_free


checkNullPointer :: String -> IO (Ptr a) -> IO (Ptr a)
checkNullPointer s k = do
  p <- k
  if p == nullPtr
  then throwErrno (s ++ " returned NULL pointer.")
  else return p
