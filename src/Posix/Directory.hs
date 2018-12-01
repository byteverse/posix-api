{-# language BangPatterns #-}
{-# language MagicHash #-}
{-# language LambdaCase #-}
{-# language UnboxedTuples #-}

module Posix.Directory
  ( getCurrentWorkingDirectory
  ) where

import Data.Primitive (Addr(..),ByteArray)
import GHC.Exts (Ptr(..))
import Foreign.Ptr (nullPtr)
import Foreign.C.Error (Errno,eRANGE,getErrno)
import Foreign.C.Types (CChar,CSize(..))
import GHC.IO (IO(..))

import qualified Data.Primitive as PM
import qualified Foreign.Storable as FS

foreign import ccall safe "getcwd"
  c_getcwd :: Ptr CChar -> CSize -> IO (Ptr CChar)

-- | Get the current working directory without using the system locale
--   to convert it to text. This is implemented with a safe FFI call
--   since it may block.
getCurrentWorkingDirectory :: IO (Either Errno ByteArray)
getCurrentWorkingDirectory = go (4096 - chunkOverhead) where
  go !sz = do
    -- It may be nice to add a variant of getCurrentWorkingDirectory that
    -- allow the user to supply an initial pinned buffer. I'm not sure
    -- how many other POSIX functions there are that could benefit
    -- from this. Calls to getCurrentWorkingDirectory are extremely rare,
    -- so there would be little benefit here, but there may be other
    -- functions where these repeated 4KB allocations might trigger
    -- GC very quickly.
    marr <- PM.newPinnedByteArray sz
    let !(Addr addr) = PM.mutableByteArrayContents marr
    ptr <- c_getcwd (Ptr addr) (intToCSize sz)
    -- We probably want to use touch# or with# here.
    if ptr /= nullPtr
      then do
        strSize <- findNullByte ptr
        dst <- PM.newByteArray strSize
        PM.copyMutableByteArray dst 0 marr 0 strSize
        dst' <- PM.unsafeFreezeByteArray dst
        pure (Right dst')
      else do
        errno <- getErrno
        if errno == eRANGE
          then go (2 * sz)
          else fmap Left getErrno

chunkOverhead :: Int
chunkOverhead = 2 * PM.sizeOf (undefined :: Int)

intToCSize :: Int -> CSize
intToCSize = fromIntegral

-- There must be a null byte present or bad things will happen.
-- This will return a nonnegative number.
findNullByte :: Ptr CChar -> IO Int
findNullByte = go 0 where
  go :: Int -> Ptr CChar -> IO Int
  go !ix !ptr = do
    FS.peekElemOff ptr ix >>= \case
      0 -> pure ix
      _ -> go (ix + 1) ptr

