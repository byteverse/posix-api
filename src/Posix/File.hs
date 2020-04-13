{-# language BangPatterns #-}
{-# language BinaryLiterals #-}
{-# language MagicHash #-}
{-# language TypeApplications #-}
{-# language UnliftedFFITypes #-}

module Posix.File
  ( -- * Functions
    uninterruptibleGetDescriptorFlags
  , uninterruptibleGetStatusFlags
  , uninterruptibleWriteByteArray
  , uninterruptibleWriteBytes
  , uninterruptibleWriteBytesCompletely
  , uninterruptibleOpen
  , uninterruptibleOpenMode
  , writeByteArray
  , close
  , uninterruptibleClose
  , uninterruptibleErrorlessClose
  , uninterruptibleUnlink
  , uninterruptibleLink
    -- * Types
  , AccessMode(..)
  , CreationFlags(..)
  , DescriptorFlags(..)
  , StatusFlags(..)
    -- * File Descriptor Flags
  , Types.nonblocking
  , Types.append
  , isReadOnly
  , isWriteOnly
  , isReadWrite
    -- * Open Access Mode
  , Types.readOnly
  , Types.writeOnly
  , Types.readWrite
    -- * File Creation Flags
  , Types.create
  , Types.truncate
  , Types.exclusive
  ) where

import Assertion (assertByteArrayPinned)
import Data.Bits ((.&.),(.|.))
import Data.Primitive (ByteArray(..))
import Foreign.C.Error (Errno,getErrno)
import Foreign.C.String.Managed (ManagedCString(..))
import Foreign.C.Types (CInt(..),CSize(..))
import GHC.Exts (ByteArray#)
import Posix.File.Types (CreationFlags(..),AccessMode(..),StatusFlags(..))
import Posix.File.Types (DescriptorFlags(..))
import System.Posix.Types (Fd(..),CSsize(..),CMode(..))
import Data.Bytes.Types (Bytes(Bytes))

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

foreign import ccall unsafe "HaskellPosix.h write_offset_loop"
  c_unsafe_bytearray_write_loop :: Fd -> ByteArray# -> Int -> CSize -> IO CInt

foreign import ccall safe "HaskellPosix.h write_offset"
  c_safe_bytearray_write :: Fd -> ByteArray# -> Int -> CSize -> IO CSsize

foreign import ccall unsafe "HaskellPosix.h open"
  c_unsafe_open :: ByteArray# -> CInt -> IO Fd

foreign import ccall unsafe "HaskellPosix.h open"
  c_unsafe_open_mode :: ByteArray# -> CInt -> CMode -> IO Fd

foreign import ccall unsafe "HaskellPosix.h unlink"
  c_unsafe_unlink :: ByteArray# -> IO CInt

foreign import ccall unsafe "HaskellPosix.h link"
  c_unsafe_link :: ByteArray# -> ByteArray# -> IO CInt

foreign import ccall safe "unistd.h close"
  c_safe_close :: Fd -> IO CInt

foreign import ccall unsafe "unistd.h close"
  c_unsafe_close :: Fd -> IO CInt

uninterruptibleOpen ::
     ManagedCString -- ^ NULL-terminated file name
  -> AccessMode -- ^ Access mode
  -> CreationFlags -- ^ Creation flags
  -> StatusFlags -- ^ Status flags
  -> IO (Either Errno Fd)
uninterruptibleOpen (ManagedCString (ByteArray name)) (AccessMode x) (CreationFlags y) (StatusFlags z) =
  c_unsafe_open name (x .|. y .|. z) >>= errorsFromFd

uninterruptibleOpenMode ::
     ManagedCString -- ^ NULL-terminated file name
  -> AccessMode -- ^ Access mode, should include @O_CREAT@
  -> CreationFlags -- ^ Creation flags
  -> StatusFlags -- ^ Status flags
  -> CMode -- ^ Permissions assigned to newly created file
  -> IO (Either Errno Fd)
uninterruptibleOpenMode (ManagedCString (ByteArray name)) (AccessMode x) (CreationFlags y) (StatusFlags z) !mode =
  c_unsafe_open_mode name (x .|. y .|. z) mode >>= errorsFromFd

errorsFromDescriptorFlags :: DescriptorFlags -> IO (Either Errno DescriptorFlags)
errorsFromDescriptorFlags r@(DescriptorFlags x) = if x > (-1)
  then pure (Right r)
  else fmap Left getErrno

errorsFromStatusFlags :: StatusFlags -> IO (Either Errno StatusFlags)
errorsFromStatusFlags r@(StatusFlags x) = if x > (-1)
  then pure (Right r)
  else fmap Left getErrno

-- | Wrapper for @write(2)@ that takes a slice of bytes and an offset.
-- The byte array backing the slice does not need to be pinned.
uninterruptibleWriteBytesCompletely ::
     Fd -- ^ File descriptor
  -> Bytes -- ^ Source bytes
  -> IO (Either Errno ())
uninterruptibleWriteBytesCompletely !fd (Bytes (ByteArray buf) off len) =
  c_unsafe_bytearray_write_loop fd buf off (fromIntegral @Int @CSize len)
    >>= errorsFromInt_

-- | Wrapper for @write(2)@ that takes a slice of bytes and an offset.
-- The byte array backing the slice does not need to be pinned.
uninterruptibleWriteBytes ::
     Fd -- ^ File descriptor
  -> Bytes -- ^ Source bytes
  -> IO (Either Errno CSize) -- ^ Number of bytes written
uninterruptibleWriteBytes !fd (Bytes (ByteArray buf) off len) =
  c_unsafe_bytearray_write fd buf off (fromIntegral @Int @CSize len)
    >>= errorsFromSize

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

-- | Wrapper for @write(2)@ that takes a byte array and an offset.
-- Uses @safe@ FFI. The byte array must be pinned.
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

errorsFromFd :: Fd -> IO (Either Errno Fd)
errorsFromFd r = if r > (-1)
  then pure (Right r)
  else fmap Left getErrno

uninterruptibleLink ::
     ManagedCString -- ^ Path to existing file
  -> ManagedCString -- ^ Path to new file
  -> IO (Either Errno ())
uninterruptibleLink (ManagedCString (ByteArray x)) (ManagedCString (ByteArray y)) =
  c_unsafe_link x y >>= errorsFromInt_

uninterruptibleUnlink ::
     ManagedCString -- ^ File name
  -> IO (Either Errno ())
uninterruptibleUnlink (ManagedCString (ByteArray x)) =
  c_unsafe_unlink x >>= errorsFromInt_

-- | Close a file descriptor.
--   The <http://pubs.opengroup.org/onlinepubs/009696899/functions/close.html POSIX specification>
--   includes more details. This uses the safe FFI.
close ::
     Fd -- ^ Socket
  -> IO (Either Errno ())
close fd = c_safe_close fd >>= errorsFromInt_

-- | Close a file descriptor. This uses the unsafe FFI. According to the
--   <http://pubs.opengroup.org/onlinepubs/009696899/functions/close.html POSIX specification>,
--   "If @fildes@ refers to a socket, @close()@ shall cause the socket to
--   be destroyed. If the socket is in connection-mode, and the @SO_LINGER@
--   option is set for the socket with non-zero linger time, and the socket
--   has untransmitted data, then @close()@ shall block for up to the current
--   linger interval until all data is transmitted."
uninterruptibleClose ::
     Fd -- ^ Socket
  -> IO (Either Errno ())
uninterruptibleClose fd = c_unsafe_close fd >>= errorsFromInt_

-- | Close a file descriptor with the unsafe FFI. Do not check for errors.
--   It is only appropriate to use this when a file descriptor is being
--   closed to handle an exceptional case. Since the user will want to
--   propogate the original exception, the exception provided by
--   'uninterruptibleClose' would just be discarded. This function allows us
--   to potentially avoid an additional FFI call to 'getErrno'.
uninterruptibleErrorlessClose ::
     Fd -- ^ Socket
  -> IO ()
uninterruptibleErrorlessClose fd = do
  _ <- c_unsafe_close fd
  pure ()

-- only call this when it is known that the argument is non-negative
cssizeToCSize :: CSsize -> CSize
cssizeToCSize = fromIntegral

isReadOnly :: StatusFlags -> Bool
isReadOnly (StatusFlags x) = x .&. 0b11 == 0

isWriteOnly :: StatusFlags -> Bool
isWriteOnly (StatusFlags x) = x .&. 0b11 == 1

isReadWrite :: StatusFlags -> Bool
isReadWrite (StatusFlags x) = x .&. 0b11 == 2

-- Sometimes, functions that return an int use zero to indicate
-- success and negative one to indicate failure without including
-- additional information in the value.
errorsFromInt_ :: CInt -> IO (Either Errno ())
errorsFromInt_ r = if r == 0
  then pure (Right ())
  else fmap Left getErrno

