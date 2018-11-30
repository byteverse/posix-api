{-# language MagicHash #-}
{-# language UnliftedFFITypes #-}

module Posix.Socket
  ( socket
  , bind
    -- * Send
  , send
  , sendUnsafe
  , sendByteArrayUnsafe
  , sendMutableByteArrayUnsafe
    -- * Receive
  , receive
  , receiveUnsafe
  , receiveMutableByteArrayUnsafe
  ) where

import GHC.Exts (RealWorld,ByteArray#,MutableByteArray#,Addr#)
import Data.Primitive (MutableByteArray(..),Addr(..),ByteArray(..))
import Foreign.C.Types (CInt(..),CSize(..))
import Posix.Socket.Types (Family(..),Protocol(..),Type(..),SocketAddress(..))
import Posix.Socket.Types (SendFlags(..),ReceiveFlags(..))
import System.Posix.Types (Fd(..),CSsize(..))
import Foreign.C.Error (Errno,getErrno)

import qualified Data.Primitive as PM

foreign import ccall safe "sys/socket.h socket"
  c_socket :: Family -> Type -> Protocol -> IO Fd

-- Per the spec, the type signature of bind is:
--   int bind(int socket, const struct sockaddr *address, socklen_t address_len);
-- However, here we choose to represent the third argument as
-- CInt rather than introducing a type corresponding to socklen_t.
-- According to Linus Torvalds, "Any sane library must have socklen_t
-- be the same size as int. Anything else breaks any BSD socket layer stuff."
-- (https://yarchive.net/comp/linux/socklen_t.html). If a platform
-- violates this assumption, this wrapper will be broken on that platform.
foreign import ccall safe "sys/socket.h bind"
  c_bind :: Fd -> ByteArray# -> CInt -> IO CInt

-- There are several options for wrapping send. Both safe and unsafe
-- are useful. Additionally, in the unsafe category, we also
-- have the option of writing to either an address or a byte array.
-- Unsafe FFI calls guarantee that byte arrays will not be relocated
-- while the FFI call is taking place.
foreign import ccall safe "sys/socket.h send"
  c_safe_addr_send :: Fd -> Addr# -> CSize -> SendFlags -> IO CSsize
foreign import ccall unsafe "sys/socket.h send"
  c_unsafe_addr_send :: Fd -> Addr# -> CSize -> SendFlags -> IO CSsize
foreign import ccall unsafe "sys/socket.h send"
  c_unsafe_bytearray_send :: Fd -> ByteArray# -> CSize -> SendFlags -> IO CSsize
foreign import ccall unsafe "sys/socket.h send"
  c_unsafe_mutable_bytearray_send :: Fd -> MutableByteArray# RealWorld -> CSize -> SendFlags -> IO CSsize

-- There are several ways to wrap recv.
foreign import ccall safe "sys/socket.h recv"
  c_safe_addr_recv :: Fd -> Addr# -> CSize -> ReceiveFlags -> IO CSsize
foreign import ccall unsafe "sys/socket.h recv"
  c_unsafe_addr_recv :: Fd -> Addr# -> CSize -> ReceiveFlags -> IO CSsize
foreign import ccall unsafe "sys/socket.h recv"
  c_unsafe_mutable_byte_array_recv :: Fd -> MutableByteArray# RealWorld -> CSize -> ReceiveFlags -> IO CSsize

-- | Create an endpoint for communication, returning a file
--   descriptor that refers to that endpoint. The
--   <http://pubs.opengroup.org/onlinepubs/9699919799/functions/socket.html POSIX specification>
--   includes more details. This is implemented as a safe FFI
--   call. It is unclear to the library author whether or not
--   it may block.
socket :: Family -> Type -> Protocol -> IO (Either Errno Fd)
socket dom typ prot = do
  r <- c_socket dom typ prot
  if r > (-1)
    then pure (Right r)
    else fmap Left getErrno

-- | Assign a local socket address address to a socket identified by descriptor socket that has
--   no local socket address assigned. The
--   <http://pubs.opengroup.org/onlinepubs/9699919799/functions/bind.html POSIX specification>
--   includes more details. This wrapper represents the @sockaddr@ pointer argument, together
--   with its @socklen_t@ size as a pinned byte array. This is unfortunate, but it
--   allows @bind@ to be used with @sockaddr@ extensions on various platforms.
bind ::
     Fd -- ^ Socket
  -> SocketAddress -- ^ Socket address, extensible tagged union
  -> IO (Either Errno ())
bind fd (SocketAddress b@(ByteArray b#)) = do
  r <- c_bind fd b# (intToCInt (PM.sizeofByteArray b))
  if r == 0
    then pure (Right ())
    else fmap Left getErrno

-- | Send data from an address over a network socket. This uses the safe FFI.
send ::
     Fd -- ^ Socket
  -> Addr -- ^ Source address
  -> CSize -- ^ Length in bytes
  -> SendFlags -- ^ Flags
  -> IO (Either Errno CSize)
send fd (Addr addr) len flags =
  c_safe_addr_send fd addr len flags >>= errorsFromSize

-- | Send data from an address over a network socket. This uses the unsafe FFI.
--   Users of this function should be sure to set flags that prohibit this
--   from blocking. On Linux this is accomplished with @O_NONBLOCK@. It is
--   often desirable to call 'threadWaitWrite' on a nonblocking socket before
--   calling @sendUnsafe@ on it.
sendUnsafe ::
     Fd -- ^ Socket
  -> Addr -- ^ Source address
  -> CSize -- ^ Length in bytes
  -> SendFlags -- ^ Flags
  -> IO (Either Errno CSize)
sendUnsafe fd (Addr addr) len flags =
  c_unsafe_addr_send fd addr len flags >>= errorsFromSize

-- | Send data from a byte array over a network socket. This uses the unsafe FFI;
--   considerations pertaining to 'sendUnsafe' apply to this function as well. Users
--   may specify a length to send fewer bytes than are actually present in the
--   array. However, there is currently no option to provide an offset.
sendByteArrayUnsafe ::
     Fd -- ^ Socket
  -> ByteArray -- ^ Source byte array
  -> CSize -- ^ Length in bytes
  -> SendFlags -- ^ Flags
  -> IO (Either Errno CSize)
sendByteArrayUnsafe fd (ByteArray b) len flags =
  c_unsafe_bytearray_send fd b len flags >>= errorsFromSize

-- | Send data from a mutable byte array over a network socket. This uses the unsafe FFI;
--   considerations pertaining to 'sendUnsafe' apply to this function as well. Users
--   may specify a length to send fewer bytes than are actually present in the
--   array. However, there is currently no option to provide an offset.
sendMutableByteArrayUnsafe ::
     Fd -- ^ Socket
  -> MutableByteArray RealWorld -- ^ Source mutable byte array
  -> CSize -- ^ Length in bytes
  -> SendFlags -- ^ Flags
  -> IO (Either Errno CSize)
sendMutableByteArrayUnsafe fd (MutableByteArray b) len flags =
  c_unsafe_mutable_bytearray_send fd b len flags >>= errorsFromSize

-- | Receive data into an address from a network socket. This wraps @recv@ using
--   the safe FFI. When the returned size is zero, there are no additional bytes
--   to receive and the peer has performed an orderly shutdown.
receive ::
     Fd -- ^ Socket
  -> Addr -- ^ Source address
  -> CSize -- ^ Length in bytes
  -> ReceiveFlags -- ^ Flags
  -> IO (Either Errno CSize)
receive fd (Addr addr) len flags =
  c_safe_addr_recv fd addr len flags >>= errorsFromSize

-- | Receive data into an address from a network socket. This wraps @recv@
--   using the unsafe FFI. Users of this function should be sure to set flags
--   that prohibit this from blocking. On Linux this is accomplished by setting
--   the @MSG_DONTWAIT@ flag and handling the resulting @EAGAIN@ or
--   @EWOULDBLOCK@. When the returned size is zero, there are no additional
--   bytes to receive and the peer has performed an orderly shutdown.
receiveUnsafe ::
     Fd -- ^ Socket
  -> Addr -- ^ Source address
  -> CSize -- ^ Length in bytes
  -> ReceiveFlags -- ^ Flags
  -> IO (Either Errno CSize)
receiveUnsafe fd (Addr addr) len flags =
  c_unsafe_addr_recv fd addr len flags >>= errorsFromSize

-- | Receive data into an address from a network socket. This uses the unsafe
--   FFI; considerations pertaining to 'receiveUnsafe' apply to this function
--   as well. Users may specify a length to receive fewer bytes than are
--   actually present in the mutable byte array. However, there is currently
--   to way to provide an offset into the array.
receiveMutableByteArrayUnsafe ::
     Fd -- ^ Socket
  -> MutableByteArray RealWorld -- ^ Source address
  -> CSize -- ^ Length in bytes
  -> ReceiveFlags -- ^ Flags
  -> IO (Either Errno CSize)
receiveMutableByteArrayUnsafe fd (MutableByteArray b) len flags =
  c_unsafe_mutable_byte_array_recv fd b len flags >>= errorsFromSize

errorsFromSize :: CSsize -> IO (Either Errno CSize)
errorsFromSize r = if r > (-1)
  then pure (Right (cssizeToCSize r))
  else fmap Left getErrno

intToCInt :: Int -> CInt
intToCInt = fromIntegral

-- only call this when it is known that the argument is non-negative
cssizeToCSize :: CSsize -> CSize
cssizeToCSize = fromIntegral

