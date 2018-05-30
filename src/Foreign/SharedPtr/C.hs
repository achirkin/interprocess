{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RoleAnnotations            #-}
module Foreign.SharedPtr.C
  ( SharedPtr (), Allocator

  , c'shared_createAllocator, c'shared_lookupAllocator
  , c'shared_destroyAllocator, c'shared_getStoreName
  , c'shared_ptrToShPtr, c'shared_shPtrToPtr
  , c'shared_malloc, c'shared_realloc, c'shared_free

  , p'shared_createAllocator, p'shared_lookupAllocator
  , p'shared_destroyAllocator, p'shared_getStoreName
  , p'shared_ptrToShPtr, p'shared_shPtrToPtr
  , p'shared_malloc, p'shared_realloc, p'shared_free
  ) where

import           Data.Data        (Data)
import           Foreign.C.String
import           Foreign.C.Types
import           Foreign.Ptr
import           Foreign.Storable
import           GHC.Generics     (Generic)

type role SharedPtr phantom
-- | Special pointer format to pass between memory spaces of processes.
newtype SharedPtr a = SharedPtr ( Ptr a )
  deriving (Eq, Ord, Show, Data, Generic, Storable)

data AllocatorT
-- | Opaque pointer to the allocator type defined in C code.
type Allocator = Ptr AllocatorT


foreign import ccall unsafe "&shared_createAllocator"
  p'shared_createAllocator
    :: FunPtr (IO Allocator)

foreign import ccall unsafe "&shared_lookupAllocator"
  p'shared_lookupAllocator
    :: FunPtr (CString -> IO Allocator)

foreign import ccall unsafe "&shared_destroyAllocator"
  p'shared_destroyAllocator
    :: FunPtr (Allocator -> IO ())

foreign import ccall unsafe "&shared_getStoreName"
  p'shared_getStoreName
    :: FunPtr (Allocator -> CString)

foreign import ccall unsafe "&shared_ptrToShPtr"
  p'shared_ptrToShPtr
    :: FunPtr (Allocator -> Ptr a -> SharedPtr a)

foreign import ccall unsafe "&shared_shPtrToPtr"
  p'shared_shPtrToPtr
    :: FunPtr (Allocator -> SharedPtr a -> Ptr a)

foreign import ccall unsafe "&shared_malloc"
  p'shared_malloc
    :: FunPtr (Allocator -> CSize -> IO (Ptr a))

foreign import ccall unsafe "&shared_realloc"
  p'shared_realloc
    :: FunPtr (Allocator -> Ptr a -> CSize -> IO (Ptr a))

foreign import ccall unsafe "&shared_free"
  p'shared_free
    :: FunPtr (Allocator -> Ptr a -> IO ())


foreign import ccall unsafe "shared_createAllocator"
  c'shared_createAllocator
    :: IO Allocator

foreign import ccall unsafe "shared_lookupAllocator"
  c'shared_lookupAllocator
    :: CString -> IO Allocator

foreign import ccall unsafe "shared_destroyAllocator"
  c'shared_destroyAllocator
    :: Allocator -> IO ()

foreign import ccall unsafe "shared_getStoreName"
  c'shared_getStoreName
    :: Allocator -> CString

foreign import ccall unsafe "shared_ptrToShPtr"
  c'shared_ptrToShPtr
    :: Allocator -> Ptr a -> SharedPtr a

foreign import ccall unsafe "shared_shPtrToPtr"
  c'shared_shPtrToPtr
    :: Allocator -> SharedPtr a -> Ptr a

foreign import ccall unsafe "shared_malloc"
  c'shared_malloc
    :: Allocator -> CSize -> IO (Ptr a)

foreign import ccall unsafe "shared_realloc"
  c'shared_realloc
    :: Allocator -> Ptr a -> CSize -> IO (Ptr a)

foreign import ccall unsafe "shared_free"
  c'shared_free
    :: Allocator -> Ptr a -> IO ()
