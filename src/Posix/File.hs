{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UnliftedFFITypes #-}

module Posix.File
  ( -- * Functions
    uninterruptibleGetDescriptorFlags
  , uninterruptibleGetStatusFlags
  , uninterruptibleWriteByteArray
  , uninterruptibleWriteBytes
  , uninterruptibleWriteBytesCompletely
  , uninterruptibleWriteBytesCompletelyErrno
  , uninterruptibleReadMutableByteArray
  , writeBytesCompletelyErrno
  , uninterruptibleOpen
  , uninterruptibleOpenMode
  , uninterruptibleOpenAtMode
  , uninterruptibleOpenUntypedFlags
  , uninterruptibleOpenModeUntypedFlags
  , uninterruptibleRenameAt
  , writeByteArray
  , writeMutableByteArray
  , close
  , uninterruptibleClose
  , uninterruptibleErrorlessClose
  , uninterruptibleUnlink
  , uninterruptibleLink

    -- * Types
  , AccessMode (..)
  , CreationFlags (..)
  , DescriptorFlags (..)
  , StatusFlags (..)

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

import Assertion (assertByteArrayPinned, assertMutableByteArrayPinned)
import Data.Bits ((.&.), (.|.))
import Data.Bytes.Types (Bytes (Bytes))
import Data.Primitive (ByteArray (..), MutableByteArray (MutableByteArray))
import Foreign.C.Error (Errno (Errno), eOK, getErrno)
import Foreign.C.String.Managed (ManagedCString (..))
import Foreign.C.Types (CInt (..), CSize (..))
import GHC.Exts (ByteArray#, MutableByteArray#, RealWorld)
import Posix.File.Types (AccessMode (..), CreationFlags (..), DescriptorFlags (..), StatusFlags (..))
import System.Posix.Types (CMode (..), CSsize (..), Fd (..))

import qualified Posix.File.Types as Types

{- | Get file descriptor flags. This uses the unsafe FFI to
perform @fcntl(fd,F_GETFD)@.
-}
uninterruptibleGetDescriptorFlags :: Fd -> IO (Either Errno DescriptorFlags)
uninterruptibleGetDescriptorFlags !fd = c_getFdFlags fd >>= errorsFromDescriptorFlags

{- | Get file status flags. This uses the unsafe FFI to
perform @fcntl(fd,F_GETFL)@.
-}
uninterruptibleGetStatusFlags :: Fd -> IO (Either Errno StatusFlags)
uninterruptibleGetStatusFlags !fd = c_getFlFlags fd >>= errorsFromStatusFlags

foreign import ccall unsafe "HaskellPosix.h hs_get_fd_flags"
  c_getFdFlags :: Fd -> IO DescriptorFlags

foreign import ccall unsafe "HaskellPosix.h hs_get_fl_flags"
  c_getFlFlags :: Fd -> IO StatusFlags

foreign import ccall unsafe "HaskellPosix.h write_offset"
  c_unsafe_bytearray_write :: Fd -> ByteArray# -> Int -> CSize -> IO CSsize

foreign import ccall unsafe "HaskellPosix.h write_offset_loop"
  c_unsafe_bytearray_write_loop :: Fd -> ByteArray# -> Int -> CSize -> IO Errno

foreign import ccall safe "HaskellPosix.h write_offset_loop"
  c_safe_bytearray_write_loop :: Fd -> ByteArray# -> Int -> CSize -> IO Errno

foreign import ccall safe "HaskellPosix.h write_offset"
  c_safe_bytearray_write :: Fd -> ByteArray# -> Int -> CSize -> IO CSsize

foreign import ccall safe "HaskellPosix.h write_offset"
  c_safe_mutablebytearray_write :: Fd -> MutableByteArray# RealWorld -> Int -> CSize -> IO CSsize

foreign import ccall unsafe "HaskellPosix.h open"
  c_unsafe_open :: ByteArray# -> CInt -> IO Fd

foreign import ccall unsafe "HaskellPosix.h renameat"
  c_unsafe_rename_at :: Fd -> ByteArray# -> Fd -> ByteArray# -> IO CInt

foreign import ccall unsafe "HaskellPosix.h open"
  c_unsafe_open_mode :: ByteArray# -> CInt -> CMode -> IO Fd

foreign import ccall unsafe "HaskellPosix.h openat"
  c_unsafe_openat_mode :: Fd -> ByteArray# -> CInt -> CMode -> IO Fd

foreign import ccall unsafe "HaskellPosix.h unlink"
  c_unsafe_unlink :: ByteArray# -> IO CInt

foreign import ccall unsafe "HaskellPosix.h link"
  c_unsafe_link :: ByteArray# -> ByteArray# -> IO CInt

foreign import ccall safe "unistd.h close"
  c_safe_close :: Fd -> IO CInt

foreign import ccall unsafe "unistd.h close"
  c_unsafe_close :: Fd -> IO CInt

-- | Rename a file. This is a wrapper around the POSIX function @renameat@.
uninterruptibleRenameAt ::
     -- | Old dir fd
     Fd
     -- | Old file name
  -> ManagedCString
     -- | New dir fd
  -> Fd
     -- | New file name
  -> ManagedCString
  -> IO (Either Errno ())
uninterruptibleRenameAt !oldDirFd (ManagedCString (ByteArray oldName)) !newDirFd (ManagedCString (ByteArray newName)) =
  c_unsafe_rename_at oldDirFd oldName newDirFd newName >>= errorsFromInt_

uninterruptibleOpen ::
  -- | NULL-terminated file name
  ManagedCString ->
  -- | Access mode
  AccessMode ->
  -- | Creation flags
  CreationFlags ->
  -- | Status flags
  StatusFlags ->
  IO (Either Errno Fd)
uninterruptibleOpen (ManagedCString (ByteArray name)) (AccessMode x) (CreationFlags y) (StatusFlags z) =
  c_unsafe_open name (x .|. y .|. z) >>= errorsFromFd

{- | Variant of 'uninterruptibleOpen' that does not help the caller with
the types of the flags.
-}
uninterruptibleOpenUntypedFlags ::
  -- | NULL-terminated file name
  ManagedCString ->
  -- | Flags
  CInt ->
  IO (Either Errno Fd)
uninterruptibleOpenUntypedFlags (ManagedCString (ByteArray name)) x =
  c_unsafe_open name x >>= errorsFromFd

{- | Variant of 'uninterruptibleOpenMode' that does not help the caller with
the types of the flags.
-}
uninterruptibleOpenModeUntypedFlags ::
  -- | NULL-terminated file name
  ManagedCString ->
  -- | Flags
  CInt ->
  -- | Mode
  CMode ->
  IO (Either Errno Fd)
uninterruptibleOpenModeUntypedFlags (ManagedCString (ByteArray name)) !x !mode =
  c_unsafe_open_mode name x mode >>= errorsFromFd

uninterruptibleOpenMode ::
  -- | NULL-terminated file name
  ManagedCString ->
  -- | Access mode, should include @O_CREAT@
  AccessMode ->
  -- | Creation flags
  CreationFlags ->
  -- | Status flags
  StatusFlags ->
  -- | Permissions assigned to newly created file
  CMode ->
  IO (Either Errno Fd)
uninterruptibleOpenMode (ManagedCString (ByteArray name)) (AccessMode x) (CreationFlags y) (StatusFlags z) !mode =
  c_unsafe_open_mode name (x .|. y .|. z) mode >>= errorsFromFd

{- | Variant of 'uninterruptibleOpenMode' that lets the user specify a
directory file descriptor instead of using the working directory as the
base path.
-}
uninterruptibleOpenAtMode ::
  -- | Base directory
  Fd ->
  -- | NULL-terminated file name
  ManagedCString ->
  -- | Access mode, should include @O_CREAT@
  AccessMode ->
  -- | Creation flags
  CreationFlags ->
  -- | Status flags
  StatusFlags ->
  -- | Permissions assigned to newly created file
  CMode ->
  IO (Either Errno Fd)
uninterruptibleOpenAtMode !dirFd (ManagedCString (ByteArray name)) (AccessMode x) (CreationFlags y) (StatusFlags z) !mode =
  c_unsafe_openat_mode dirFd name (x .|. y .|. z) mode >>= errorsFromFd

errorsFromDescriptorFlags :: DescriptorFlags -> IO (Either Errno DescriptorFlags)
errorsFromDescriptorFlags r@(DescriptorFlags x) =
  if x > (-1)
    then pure (Right r)
    else fmap Left getErrno

errorsFromStatusFlags :: StatusFlags -> IO (Either Errno StatusFlags)
errorsFromStatusFlags r@(StatusFlags x) =
  if x > (-1)
    then pure (Right r)
    else fmap Left getErrno

{- | Wrapper for @write(2)@ that takes a slice of bytes and an offset.
The byte array backing the slice does not need to be pinned.
-}
uninterruptibleWriteBytesCompletely ::
  -- | File descriptor
  Fd ->
  -- | Source bytes
  Bytes ->
  IO (Either Errno ())
uninterruptibleWriteBytesCompletely !fd !b = do
  e <- uninterruptibleWriteBytesCompletelyErrno fd b
  if e == eOK
    then pure (Right ())
    else pure (Left e)

{- | Variant of 'uninterruptibleWriteBytesCompletely' that uses errno 0
to communicate success.
-}
uninterruptibleWriteBytesCompletelyErrno ::
  -- | File descriptor
  Fd ->
  -- | Source bytes
  Bytes ->
  IO Errno
uninterruptibleWriteBytesCompletelyErrno !fd (Bytes (ByteArray buf) off len) =
  c_unsafe_bytearray_write_loop fd buf off (fromIntegral @Int @CSize len)

{- | Wrapper for @write(2)@ that takes a slice of bytes and an offset.
The byte array backing the slice must be pinned.
-}
writeBytesCompletelyErrno ::
  -- | File descriptor
  Fd ->
  -- | Source bytes
  Bytes ->
  IO Errno
writeBytesCompletelyErrno !fd (Bytes buf0 off len) =
  let !(ByteArray buf1) = assertByteArrayPinned buf0
   in c_safe_bytearray_write_loop fd buf1 off (fromIntegral @Int @CSize len)

{- | Wrapper for @write(2)@ that takes a slice of bytes and an offset.
The byte array backing the slice does not need to be pinned.
-}
uninterruptibleWriteBytes ::
  -- | File descriptor
  Fd ->
  -- | Source bytes
  Bytes ->
  -- | Number of bytes written
  IO (Either Errno CSize)
uninterruptibleWriteBytes !fd (Bytes (ByteArray buf) off len) =
  c_unsafe_bytearray_write fd buf off (fromIntegral @Int @CSize len)
    >>= errorsFromSize

{- | Wrapper for @write(2)@ that takes a byte array and an offset.
The byte array does not need to be pinned.
-}
uninterruptibleWriteByteArray ::
  -- | Socket
  Fd ->
  -- | Source byte array
  ByteArray ->
  -- | Offset into source array
  Int ->
  -- | Length in bytes
  CSize ->
  -- | Number of bytes pushed to send buffer
  IO (Either Errno CSize)
uninterruptibleWriteByteArray !fd (ByteArray buf) !off !len =
  c_unsafe_bytearray_write fd buf off len >>= errorsFromSize

{- | Wrapper for @write(2)@ that takes a byte array and an offset.
Uses @safe@ FFI. The byte array must be pinned.
-}
writeByteArray ::
  -- | File descriptor
  Fd ->
  -- | Source byte array
  ByteArray ->
  -- | Offset into source array
  Int ->
  -- | Length in bytes
  CSize ->
  -- | Number of bytes pushed to send buffer
  IO (Either Errno CSize)
writeByteArray !fd !buf0 !off !len =
  let !(ByteArray buf1) = assertByteArrayPinned buf0
   in c_safe_bytearray_write fd buf1 off len >>= errorsFromSize

-- writeByteArrayCompletely ::

{- | Variant of 'writeByteArray' that operates on mutable byte array.
Uses @safe@ FFI. The byte array must be pinned.
-}
writeMutableByteArray ::
  -- | File descriptor
  Fd ->
  -- | Source byte array
  MutableByteArray RealWorld ->
  -- | Offset into source array
  Int ->
  -- | Length in bytes
  CSize ->
  -- | Number of bytes pushed to send buffer
  IO (Either Errno CSize)
writeMutableByteArray !fd !buf0 !off !len =
  let !(MutableByteArray buf1) = assertMutableByteArrayPinned buf0
   in c_safe_mutablebytearray_write fd buf1 off len >>= errorsFromSize

uninterruptibleReadMutableByteArray ::
  -- | File descriptor
  Fd ->
  -- | Destination
  MutableByteArray RealWorld ->
  -- | Destination offset
  Int ->
  -- | Length in bytes
  CSize ->
  -- | Number of bytes received
  IO (Either Errno CSize)
uninterruptibleReadMutableByteArray !fd !(MutableByteArray !b) !doff !dlen = do
  c_unsafe_mutable_byte_array_read fd b doff dlen >>= errorsFromSize

errorsFromSize :: CSsize -> IO (Either Errno CSize)
errorsFromSize r =
  if r > (-1)
    then pure (Right (cssizeToCSize r))
    else fmap Left getErrno

errorsFromFd :: Fd -> IO (Either Errno Fd)
errorsFromFd r =
  if r > (-1)
    then pure (Right r)
    else fmap Left getErrno

uninterruptibleLink ::
  -- | Path to existing file
  ManagedCString ->
  -- | Path to new file
  ManagedCString ->
  IO (Either Errno ())
uninterruptibleLink (ManagedCString (ByteArray x)) (ManagedCString (ByteArray y)) =
  c_unsafe_link x y >>= errorsFromInt_

uninterruptibleUnlink ::
  -- | File name
  ManagedCString ->
  IO (Either Errno ())
uninterruptibleUnlink (ManagedCString (ByteArray x)) =
  c_unsafe_unlink x >>= errorsFromInt_

{- | Close a file descriptor.
  The <http://pubs.opengroup.org/onlinepubs/009696899/functions/close.html POSIX specification>
  includes more details. This uses the safe FFI.
-}
close ::
  -- | Socket
  Fd ->
  IO (Either Errno ())
close fd = c_safe_close fd >>= errorsFromInt_

{- | Close a file descriptor. This uses the unsafe FFI. According to the
  <http://pubs.opengroup.org/onlinepubs/009696899/functions/close.html POSIX specification>,
  "If @fildes@ refers to a socket, @close()@ shall cause the socket to
  be destroyed. If the socket is in connection-mode, and the @SO_LINGER@
  option is set for the socket with non-zero linger time, and the socket
  has untransmitted data, then @close()@ shall block for up to the current
  linger interval until all data is transmitted."
-}
uninterruptibleClose ::
  -- | Socket
  Fd ->
  IO (Either Errno ())
uninterruptibleClose fd = c_unsafe_close fd >>= errorsFromInt_

{- | Close a file descriptor with the unsafe FFI. Do not check for errors.
  It is only appropriate to use this when a file descriptor is being
  closed to handle an exceptional case. Since the user will want to
  propogate the original exception, the exception provided by
  'uninterruptibleClose' would just be discarded. This function allows us
  to potentially avoid an additional FFI call to 'getErrno'.
-}
uninterruptibleErrorlessClose ::
  -- | Socket
  Fd ->
  IO ()
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
errorsFromInt_ r =
  if r == 0
    then pure (Right ())
    else fmap Left getErrno

foreign import ccall unsafe "HaskellPosix.h read_offset"
  c_unsafe_mutable_byte_array_read :: Fd -> MutableByteArray# RealWorld -> Int -> CSize -> IO CSsize
