{-# language BangPatterns #-}
{-# language BinaryLiterals #-}
{-# language MagicHash #-}
{-# language UnliftedFFITypes #-}

module Posix.File
  ( -- * Functions
    uninterruptibleGetDescriptorFlags
  , uninterruptibleGetStatusFlags
  , uninterruptibleWriteByteArray
  , writeByteArray
    -- * Types
  , DescriptorFlags(..)
  , StatusFlags(..)
    -- * File Descriptor Flags
  , Types.nonblocking
  , Types.append
  , isReadOnly
  , isWriteOnly
  , isReadWrite
  ) where

import Assertion (assertByteArrayPinned)
import Data.Bits ((.&.))
import Posix.File.Types (DescriptorFlags(..),StatusFlags(..))
import System.Posix.Types (Fd(..),CSsize(..))
import Foreign.C.Error (Errno,getErrno)
import Foreign.C.Types (CInt(..),CSize(..))
import Data.Primitive (ByteArray(..))
import GHC.Exts (ByteArray#)

import qualified Posix.File.Types as Types

-- | Get file descriptor flags. This uses the unsafe FFI to
-- perform @fcntl(fd,F_GETFD)@.
uninterruptibleGetDescriptorFlags :: Fd -> IO (Either Errno DescriptorFlags)
uninterruptibleGetDescriptorFlags !fd = c_getFdFlags fd >>= errorsFromDescriptorFlags

-- | Get file status flags. This uses the unsafe FFI to
-- perform @fcntl(fd,F_GETFL)@.
uninterruptibleGetStatusFlags :: Fd -> IO (Either Errno StatusFlags)
uninterruptibleGetStatusFlags !fd = c_getFlFlags fd >>= errorsFromStatusFlags

foreign import ccall unsafe "HaskellPosix.h hs_get_fd_flags"
  c_getFdFlags :: Fd -> IO DescriptorFlags

foreign import ccall unsafe "HaskellPosix.h hs_get_fl_flags"
  c_getFlFlags :: Fd -> IO StatusFlags

foreign import ccall unsafe "HaskellPosix.h write_offset"
  c_unsafe_bytearray_write :: Fd -> ByteArray# -> Int -> CSize -> IO CSsize

foreign import ccall safe "HaskellPosix.h write_offset"
  c_safe_bytearray_write :: Fd -> ByteArray# -> Int -> CSize -> IO CSsize

errorsFromDescriptorFlags :: DescriptorFlags -> IO (Either Errno DescriptorFlags)
errorsFromDescriptorFlags r@(DescriptorFlags x) = if x > (-1)
  then pure (Right r)
  else fmap Left getErrno

errorsFromStatusFlags :: StatusFlags -> IO (Either Errno StatusFlags)
errorsFromStatusFlags r@(StatusFlags x) = if x > (-1)
  then pure (Right r)
  else fmap Left getErrno

-- | Wrapper for @write(2)@ that takes a byte array and an offset.
-- The byte array does not need to be pinned.
uninterruptibleWriteByteArray :: 
     Fd -- ^ Socket
  -> ByteArray -- ^ Source byte array
  -> Int -- ^ Offset into source array
  -> CSize -- ^ Length in bytes
  -> IO (Either Errno CSize) -- ^ Number of bytes pushed to send buffer
uninterruptibleWriteByteArray !fd (ByteArray buf) !off !len =
  c_unsafe_bytearray_write fd buf off len >>= errorsFromSize

-- | Wrapper for @write(2)@ that takes a byte array and an offset. Uses @safe@ FFI. The byte array must be pinned.
writeByteArray :: 
     Fd -- ^ Socket
  -> ByteArray -- ^ Source byte array
  -> Int -- ^ Offset into source array
  -> CSize -- ^ Length in bytes
  -> IO (Either Errno CSize) -- ^ Number of bytes pushed to send buffer
writeByteArray !fd !buf0 !off !len =
  let !(ByteArray buf1) = assertByteArrayPinned buf0
   in c_safe_bytearray_write fd buf1 off len >>= errorsFromSize

errorsFromSize :: CSsize -> IO (Either Errno CSize)
errorsFromSize r = if r > (-1)
  then pure (Right (cssizeToCSize r))
  else fmap Left getErrno

-- only call this when it is known that the argument is non-negative
cssizeToCSize :: CSsize -> CSize
cssizeToCSize = fromIntegral

isReadOnly :: StatusFlags -> Bool
isReadOnly (StatusFlags x) = x .&. 0b11 == 0

isWriteOnly :: StatusFlags -> Bool
isWriteOnly (StatusFlags x) = x .&. 0b11 == 1

isReadWrite :: StatusFlags -> Bool
isReadWrite (StatusFlags x) = x .&. 0b11 == 2
