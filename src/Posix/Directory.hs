{-# language BangPatterns #-}
{-# language MagicHash #-}
{-# language LambdaCase #-}
{-# language UnboxedTuples #-}

module Posix.Directory
  ( getCurrentWorkingDirectory
  ) where

import Data.Primitive (Addr(..),ByteArray)
import GHC.Exts (Ptr(..),Int(..),RealWorld,touch#,byteArrayContents#,unsafeFreezeByteArray#)
import GHC.Exts (newPinnedByteArray#,isMutableByteArrayPinned#)
import Foreign.Ptr (nullPtr)
import Foreign.C.Error (throwErrno,eRANGE,getErrno)
import Foreign.C.Types (CChar,CSize(..))
import GHC.IO (IO(..))
import Control.Monad.Primitive (primitive_)

import qualified Data.Primitive as PM
import qualified Foreign.Storable as FS
import qualified Foreign.Marshal.Alloc as FMA

foreign import ccall safe "getcwd"
  c_getcwd :: Ptr CChar -> CSize -> IO (Ptr CChar)

-- | Get the current working directory without using the system locale
--   to convert it to text. This is implemented with a safe FFI call
--   since it may block.
getCurrentWorkingDirectory :: IO ByteArray
getCurrentWorkingDirectory = go (4096 - chunkOverhead) where
  go !sz = do
    marr <- PM.newByteArray sz
    strSize <- withMutableByteArray marr $ \(Addr addr) -> do
      ptr <- c_getcwd (Ptr addr) (intToCSize sz)
      if ptr /= nullPtr
        then findNullByte ptr
        else do
          errno <- getErrno
          if errno == eRANGE
            then pure (-1)
            else throwErrno "Posix.Directory.getCurrentWorkingDirectory"
    if strSize == (-1)
      then go (2 * sz)
      else do
        marr' <- PM.resizeMutableByteArray marr strSize
        PM.unsafeFreezeByteArray marr'

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

withMutableByteArray ::
     PM.MutableByteArray RealWorld
  -> (PM.Addr -> IO b) -- ^ buffer filling function
  -> IO b
{-# INLINE withMutableByteArray #-}
withMutableByteArray marr@(PM.MutableByteArray marr#) f =
  case isMutableByteArrayPinned# marr# of
    1# -> do
      let !addr = PM.mutableByteArrayContents marr
      r <- f addr
      -- TODO: Replace this use of touch# with with# once it is
      -- released. For now, be careful to only use this with
      -- buffer-filling functions that GHC does not believe
      -- will certainly error. This is pretty easy to do in practice.
      -- Just do not call throwIO or error.
      primitive_ (touch# marr#)
      pure r
    _ -> do
      n <- PM.getSizeofMutableByteArray marr
      inlineAllocaBytes n $ \addr -> do
        !r <- f addr
        PM.copyAddrToByteArray marr 0 addr n
        return r

-- We redefine allocaBytes here to remove the NOINLINE pragma present
-- until base-4.12. We know that we are not doing anything dangerous
-- here since we do not feed throwIO into the callback.
inlineAllocaBytes :: Int -> (Addr -> IO b) -> IO b
inlineAllocaBytes (I# size) action = IO $ \ s0 ->
     case newPinnedByteArray# size s0      of { (# s1, mbarr# #) ->
     case unsafeFreezeByteArray# mbarr# s1 of { (# s2, barr#  #) ->
     let addr = Addr (byteArrayContents# barr#) in
     case action addr     of { IO action' ->
     case action' s2      of { (# s3, r #) ->
     case touch# barr# s3 of { s4 ->
     (# s4, r #)
  }}}}}


