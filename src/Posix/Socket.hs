{-# language MagicHash #-}
{-# language ScopedTypeVariables #-}
{-# language UnliftedFFITypes #-}

-- | Types and functions related to the POSIX sockets API.
--   Unusual characteristics:
--
--   * Any time the standard calls for @socklen_t@, we use
--     @CInt@ instead. Linus Torvalds <https://yarchive.net/comp/linux/socklen_t.html writes>
--     that \"Any sane library must have @socklen_t@ be the same size as @int@.
--     Anything else breaks any BSD socket layer stuff.\"
module Posix.Socket
  ( -- * Socket
    socket
    -- * Bind
  , bind
    -- * Connect
  , connect
    -- * Listen
  , listen
    -- * Accept
  , accept
  , accept_
    -- * Send
  , send
  , unsafeSend
  , unsafeSendByteArray
  , unsafeSendMutableByteArray
    -- * Receive
  , receive
  , unsafeReceive
  , unsafeReceiveMutableByteArray
  ) where

import Data.Primitive (MutableByteArray(..),Addr(..),ByteArray(..))
import Data.Void (Void)
import Foreign.C.Error (Errno,getErrno)
import Foreign.C.Types (CInt(..),CSize(..))
import Foreign.Ptr (nullPtr)
import GHC.Exts (Ptr,RealWorld,ByteArray#,MutableByteArray#,Addr#)
import Posix.Socket.Types (Family(..),Protocol(..),Type(..),SocketAddress(..))
import Posix.Socket.Types (SendFlags(..),ReceiveFlags(..))
import System.Posix.Types (Fd(..),CSsize(..))

import qualified Data.Primitive as PM

foreign import ccall safe "sys/socket.h socket"
  c_socket :: Family -> Type -> Protocol -> IO Fd

foreign import ccall unsafe "sys/socket.h listen"
  c_listen :: Fd -> CInt -> IO CInt

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

-- Per the spec, the type signature of accept is:
--   int accept(int socket, struct sockaddr *restrict address, socklen_t *restrict address_len);
-- The restrict keyword does not matter much for our purposes. See the
-- note on c_bind for why we use CInt for socklen_t. Remember that the
-- first bytearray argument is actually SocketAddress in the function that
-- wraps this one. The second bytearray argument is a pointer to the size.
foreign import ccall safe "sys/socket.h accept"
  c_safe_accept :: Fd
                -> MutableByteArray# RealWorld -- SocketAddress
                -> MutableByteArray# RealWorld -- Ptr CInt
                -> IO Fd
foreign import ccall safe "sys/socket.h accept"
  c_safe_ptr_accept :: Fd -> Ptr Void -> Ptr CInt -> IO Fd

-- Per the spec the type signature of connect is:
--   int connect(int sockfd, const struct sockaddr *addr, socklen_t addrlen);
-- The bytearray argument is actually SocketAddress.
foreign import ccall safe "sys/socket.h connect"
  c_safe_connect :: Fd -> ByteArray# -> CInt -> IO CInt

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

-- | Assign a local socket address address to a socket identified by
--   descriptor socket that has no local socket address assigned. The
--   <http://pubs.opengroup.org/onlinepubs/9699919799/functions/bind.html POSIX specification>
--   includes more details. The 'SocketAddress' represents the @sockaddr@ pointer argument, together
--   with its @socklen_t@ size, as a byte array. This allows @bind@ to
--   be used with @sockaddr@ extensions on various platforms.
bind ::
     Fd -- ^ Socket
  -> SocketAddress -- ^ Socket address, extensible tagged union
  -> IO (Either Errno ())
bind fd (SocketAddress b@(ByteArray b#)) =
  c_bind fd b# (intToCInt (PM.sizeofByteArray b)) >>= errorsFromInt

-- | Mark the socket as a passive socket, that is, as a socket that
--   will be used to accept incoming connection requests using @accept@.
--   The <http://pubs.opengroup.org/onlinepubs/9699919799/functions/listen.html POSIX specification>
--   includes more details. Listen uses the unsafe FFI since it cannot block
--   and always returns promptly.
listen ::
     Fd -- ^ Socket
  -> CInt -- ^ Backlog
  -> IO (Either Errno ())
listen fd backlog = c_listen fd backlog >>= errorsFromInt

-- | Connect the socket to the specified socket address.
--   The <http://pubs.opengroup.org/onlinepubs/9699919799/functions/connect.html POSIX specification>
--   includes more details. An @unsafeConnect@ using the unsafe FFI is
--   not provided since there is no way to hook such a beast into the
--   event manager.
connect ::
     Fd -- ^ Fd
  -> SocketAddress -- ^ Socket address, extensible tagged union
  -> IO (Either Errno ())
connect fd (SocketAddress sockAddr@(ByteArray sockAddr#)) =
  c_safe_connect fd sockAddr# (intToCInt (PM.sizeofByteArray sockAddr)) >>= errorsFromInt

-- | Extract the first connection on the queue of pending connections. The
--   <http://pubs.opengroup.org/onlinepubs/9699919799/functions/accept.html POSIX specification>
--   includes more details. This function\'s type differs slightly from
--   the specification:
--
--   > int accept(int socket, struct sockaddr *restrict address, socklen_t *restrict address_len);
--
--   Instead of requiring the caller to prepare buffers through which
--   information is returned, this haskell binding to @accept@ prepares
--   those buffers internally. This eschews C\'s characteristic buffer-passing
--   in favor of the Haskell convention of allocating internally and returning.
--
--   More specifically, this binding lacks an argument corresponding to the
--   @sockaddr@ buffer from the specification. That mutable buffer is allocated
--   internally, resized and frozen upon a success, and returned along with
--   the file descriptor of the accepted socket. The size of this buffer is
--   determined by the second argument (maximum socket address size). This
--   size argument is also writen to the @address_len@ buffer, which is also
--   allocated internally. The size returned through this pointer is used to
--   resize the @sockaddr@ buffer, which is then frozen so that an immutable
--   'SocketAddress' is returned to the end user.
--
--   For applications uninterested in the peer (described by @sockaddr@),
--   POSIX @accept@ allows the null pointer to be passed as both @address@ and
--   @address_len@. This behavior is provided by 'accept_'.
accept ::
     Fd -- ^ Listening socket
  -> CInt -- ^ Maximum socket address size
  -> IO (Either Errno (SocketAddress,Fd)) -- ^ Peer information and connected socket
accept sock maxSz = do
  sockAddrBuf@(MutableByteArray sockAddrBuf#) <- PM.newPinnedByteArray (cintToInt maxSz)
  lenBuf@(MutableByteArray lenBuf#) <- PM.newPinnedByteArray (PM.sizeOf (undefined :: CInt))
  PM.writeByteArray lenBuf 0 maxSz
  r <- c_safe_accept sock sockAddrBuf# lenBuf#
  if r > (-1)
    then do
      (sz :: CInt) <- PM.readByteArray lenBuf 0
      sockAddr <- PM.unsafeFreezeByteArray =<< PM.resizeMutableByteArray sockAddrBuf (cintToInt sz)
      pure (Right (SocketAddress sockAddr,r))
    else fmap Left getErrno

-- | A variant of 'accept' that does not provide the user with a
--   'SocketAddress' detailing the peer.
accept_ ::
     Fd -- ^ Listening socket
  -> IO (Either Errno Fd) -- ^ Connected socket
accept_ sock =
  c_safe_ptr_accept sock nullPtr nullPtr >>= errorsFromFd
  

-- | Send data from an address over a network socket. This uses the safe FFI.
send ::
     Fd -- ^ Connected socket
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
--   calling @unsafeSend@ on it.
unsafeSend ::
     Fd -- ^ Socket
  -> Addr -- ^ Source address
  -> CSize -- ^ Length in bytes
  -> SendFlags -- ^ Flags
  -> IO (Either Errno CSize)
unsafeSend fd (Addr addr) len flags =
  c_unsafe_addr_send fd addr len flags >>= errorsFromSize

-- | Send data from a byte array over a network socket. This uses the unsafe FFI;
--   considerations pertaining to 'sendUnsafe' apply to this function as well. Users
--   may specify a length to send fewer bytes than are actually present in the
--   array. However, there is currently no option to provide an offset.
unsafeSendByteArray ::
     Fd -- ^ Socket
  -> ByteArray -- ^ Source byte array
  -> CSize -- ^ Length in bytes
  -> SendFlags -- ^ Flags
  -> IO (Either Errno CSize)
unsafeSendByteArray fd (ByteArray b) len flags =
  c_unsafe_bytearray_send fd b len flags >>= errorsFromSize

-- | Send data from a mutable byte array over a network socket. This uses the unsafe FFI;
--   considerations pertaining to 'sendUnsafe' apply to this function as well. Users
--   may specify a length to send fewer bytes than are actually present in the
--   array. However, there is currently no option to provide an offset.
unsafeSendMutableByteArray ::
     Fd -- ^ Socket
  -> MutableByteArray RealWorld -- ^ Source mutable byte array
  -> CSize -- ^ Length in bytes
  -> SendFlags -- ^ Flags
  -> IO (Either Errno CSize)
unsafeSendMutableByteArray fd (MutableByteArray b) len flags =
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
unsafeReceive ::
     Fd -- ^ Socket
  -> Addr -- ^ Source address
  -> CSize -- ^ Length in bytes
  -> ReceiveFlags -- ^ Flags
  -> IO (Either Errno CSize)
unsafeReceive fd (Addr addr) len flags =
  c_unsafe_addr_recv fd addr len flags >>= errorsFromSize

-- | Receive data into an address from a network socket. This uses the unsafe
--   FFI; considerations pertaining to 'receiveUnsafe' apply to this function
--   as well. Users may specify a length to receive fewer bytes than are
--   actually present in the mutable byte array. However, there is currently
--   to way to provide an offset into the array.
unsafeReceiveMutableByteArray ::
     Fd -- ^ Socket
  -> MutableByteArray RealWorld -- ^ Source address
  -> CSize -- ^ Length in bytes
  -> ReceiveFlags -- ^ Flags
  -> IO (Either Errno CSize)
unsafeReceiveMutableByteArray fd (MutableByteArray b) len flags =
  c_unsafe_mutable_byte_array_recv fd b len flags >>= errorsFromSize

errorsFromSize :: CSsize -> IO (Either Errno CSize)
errorsFromSize r = if r > (-1)
  then pure (Right (cssizeToCSize r))
  else fmap Left getErrno

errorsFromFd :: Fd -> IO (Either Errno Fd)
errorsFromFd r = if r > (-1)
  then pure (Right r)
  else fmap Left getErrno

-- Sometimes, functions that return an int use zero to indicate
-- success and negative one to indicate failure without including
-- additional information in the value.
errorsFromInt :: CInt -> IO (Either Errno ())
errorsFromInt r = if r == 1
  then pure (Right ())
  else fmap Left getErrno

intToCInt :: Int -> CInt
intToCInt = fromIntegral

cintToInt :: CInt -> Int
cintToInt = fromIntegral

-- only call this when it is known that the argument is non-negative
cssizeToCSize :: CSsize -> CSize
cssizeToCSize = fromIntegral


