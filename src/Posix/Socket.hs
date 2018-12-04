{-# language BangPatterns #-}
{-# language InterruptibleFFI #-}
{-# language MagicHash #-}
{-# language ScopedTypeVariables #-}
{-# language UnliftedFFITypes #-}
{-# language UnboxedTuples #-}

-- | Types and functions related to the POSIX sockets API.
--   Unusual characteristics:
--
--   * Any time the standard calls for @socklen_t@, we use
--     @CInt@ instead. Linus Torvalds <https://yarchive.net/comp/linux/socklen_t.html writes>
--     that \"Any sane library must have @socklen_t@ be the same size as @int@.
--     Anything else breaks any BSD socket layer stuff.\"
--   * Send and receive each have several variants. They are distinguished by
--     the safe/unsafe FFI use and by the @Addr@/@ByteArray@/@MutableByteArray@
--     buffer type. They all call @send@ or @recv@ exactly once. They do not
--     repeatedly make syscalls like some of the functions in @network@.
--     Users who want that behavior need to build on top of this package.
module Posix.Socket
  ( -- * Socket
    socket
    -- * Socket Pair
  , socketPair
    -- * Bind
  , bind
    -- * Connect
  , connect
    -- * Listen
  , listen
    -- * Accept
  , accept
  , accept_
    -- * Close
  , close
  , unsafeClose
    -- * Send
  , send
  , sendByteArray
  , sendMutableByteArray
  , unsafeSend
  , unsafeSendByteArray
  , unsafeSendMutableByteArray
    -- * Receive
  , receive
  , receiveByteArray
  , unsafeReceive
  , unsafeReceiveMutableByteArray
  ) where

import GHC.IO (IO(..))
import Data.Primitive (MutablePrimArray(..),MutableByteArray(..),Addr(..),ByteArray(..))
import Data.Void (Void)
import Foreign.C.Error (Errno,getErrno)
import Foreign.C.Types (CInt(..),CSize(..))
import Foreign.Ptr (nullPtr)
import GHC.Exts (Ptr,RealWorld,ByteArray#,MutableByteArray#,Addr#)
import Posix.Socket.Types (Domain(..),Protocol(..),Type(..),SocketAddress(..))
import Posix.Socket.Types (SendFlags(..),ReceiveFlags(..))
import System.Posix.Types (Fd(..),CSsize(..))

import qualified Data.Primitive as PM

foreign import ccall unsafe "sys/socket.h socket"
  c_socket :: Domain -> Type -> Protocol -> IO Fd

foreign import ccall unsafe "sys/socket.h socketpair"
  c_socketpair :: Domain -> Type -> Protocol -> MutableByteArray# RealWorld -> IO CInt

foreign import ccall unsafe "sys/socket.h listen"
  c_listen :: Fd -> CInt -> IO CInt

foreign import ccall interruptible "unistd.h close"
  c_safe_close :: Fd -> IO CInt

foreign import ccall unsafe "unistd.h close"
  c_unsafe_close :: Fd -> IO CInt

-- Per the spec, the type signature of bind is:
--   int bind(int socket, const struct sockaddr *address, socklen_t address_len);
-- However, here we choose to represent the third argument as
-- CInt rather than introducing a type corresponding to socklen_t.
-- According to Linus Torvalds, "Any sane library must have socklen_t
-- be the same size as int. Anything else breaks any BSD socket layer stuff."
-- (https://yarchive.net/comp/linux/socklen_t.html). If a platform
-- violates this assumption, this wrapper will be broken on that platform.
foreign import ccall unsafe "sys/socket.h bind"
  c_bind :: Fd -> ByteArray# -> CInt -> IO CInt

-- Per the spec, the type signature of accept is:
--   int accept(int socket, struct sockaddr *restrict address, socklen_t *restrict address_len);
-- The restrict keyword does not matter much for our purposes. See the
-- note on c_bind for why we use CInt for socklen_t. Remember that the
-- first bytearray argument is actually SocketAddress in the function that
-- wraps this one. The second bytearray argument is a pointer to the size.
foreign import ccall interruptible "sys/socket.h accept"
  c_safe_accept :: Fd
                -> MutableByteArray# RealWorld -- SocketAddress
                -> MutableByteArray# RealWorld -- Ptr CInt
                -> IO Fd
foreign import ccall interruptible "sys/socket.h accept"
  c_safe_ptr_accept :: Fd -> Ptr Void -> Ptr CInt -> IO Fd

-- Per the spec the type signature of connect is:
--   int connect(int sockfd, const struct sockaddr *addr, socklen_t addrlen);
-- The bytearray argument is actually SocketAddress.
foreign import ccall interruptible "sys/socket.h connect"
  c_safe_connect :: Fd -> ByteArray# -> CInt -> IO CInt

-- There are several options for wrapping send. Both safe and unsafe
-- are useful. Additionally, in the unsafe category, we also
-- have the option of writing to either an address or a byte array.
-- Unsafe FFI calls guarantee that byte arrays will not be relocated
-- while the FFI call is taking place. Safe FFI calls do not have
-- this guarantee, so internally we must be careful when using these to only
-- provide pinned byte arrays as arguments.
foreign import ccall interruptible "sys/socket.h send"
  c_safe_addr_send :: Fd -> Addr# -> CSize -> SendFlags -> IO CSsize
foreign import ccall interruptible "sys/socket.h send_offset"
  c_safe_bytearray_send :: Fd -> ByteArray# -> CInt -> CSize -> SendFlags -> IO CSsize
foreign import ccall interruptible "sys/socket.h send_offset"
  c_safe_mutablebytearray_send :: Fd -> MutableByteArray# RealWorld -> CInt -> CSize -> SendFlags -> IO CSsize
foreign import ccall interruptible "sys/socket.h send"
  c_safe_mutablebytearray_no_offset_send :: Fd -> MutableByteArray# RealWorld -> CSize -> SendFlags -> IO CSsize
foreign import ccall unsafe "sys/socket.h send"
  c_unsafe_addr_send :: Fd -> Addr# -> CSize -> SendFlags -> IO CSsize
foreign import ccall unsafe "sys/socket.h send_offset"
  c_unsafe_bytearray_send :: Fd -> ByteArray# -> CInt -> CSize -> SendFlags -> IO CSsize
foreign import ccall unsafe "sys/socket.h send_offset"
  c_unsafe_mutable_bytearray_send :: Fd -> MutableByteArray# RealWorld -> CInt -> CSize -> SendFlags -> IO CSsize

-- There are several ways to wrap recv.
foreign import ccall interruptible "sys/socket.h recv"
  c_safe_addr_recv :: Fd -> Addr# -> CSize -> ReceiveFlags -> IO CSsize
foreign import ccall unsafe "sys/socket.h recv"
  c_unsafe_addr_recv :: Fd -> Addr# -> CSize -> ReceiveFlags -> IO CSsize
foreign import ccall unsafe "sys/socket.h recv_offset"
  c_unsafe_mutable_byte_array_recv :: Fd -> MutableByteArray# RealWorld -> CInt -> CSize -> ReceiveFlags -> IO CSsize

-- | Create an endpoint for communication, returning a file
--   descriptor that refers to that endpoint. The
--   <http://pubs.opengroup.org/onlinepubs/9699919799/functions/socket.html POSIX specification>
--   includes more details. This is implemented as a unsafe FFI
--   call since the author believes that it cannot block indefinitely.
socket ::
     Domain -- ^ Communications domain (e.g. 'internet', 'unix')
  -> Type -- ^ Socket type (e.g. 'datagram', 'stream')
  -> Protocol -- ^ Protocol
  -> IO (Either Errno Fd)
socket dom typ prot = c_socket dom typ prot >>= errorsFromFd

-- | Create an unbound pair of connected sockets in a specified domain, of
--   a specified type, under the protocol optionally specified by the protocol
--   argument. The <http://pubs.opengroup.org/onlinepubs/9699919799/functions/socketpair.html POSIX specification>
--   includes more details. This is implemented as an unsafe FFI call
--   since the author believes that it cannot block indefinitely.
socketPair ::
     Domain -- ^ Communications domain (probably 'unix')
  -> Type -- ^ Socket type (e.g. 'datagram', 'stream')
  -> Protocol -- ^ Protocol
  -> IO (Either Errno (Fd,Fd))
socketPair dom typ prot = do
  -- If this ever switches to the safe FFI, we will need to use
  -- a pinned array here instead.
  (sockets@(MutablePrimArray sockets#) :: MutablePrimArray RealWorld Fd) <- PM.newPrimArray 2
  r <- c_socketpair dom typ prot sockets#
  if r == 0
    then do
      fd1 <- PM.readPrimArray sockets 0
      fd2 <- PM.readPrimArray sockets 1
      pure (Right (fd1,fd2))
    else fmap Left getErrno

-- | Assign a local socket address address to a socket identified by
--   descriptor socket that has no local socket address assigned. The
--   <http://pubs.opengroup.org/onlinepubs/9699919799/functions/bind.html POSIX specification>
--   includes more details. The 'SocketAddress' represents the @sockaddr@ pointer argument, together
--   with its @socklen_t@ size, as a byte array. This allows @bind@ to
--   be used with @sockaddr@ extensions on various platforms. This uses
--   the unsafe FFI since the author believes it cannot block indefinitely.
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

-- | Send data from a byte array over a network socket. Users
--   may specify an offset and a length to send fewer bytes than are
--   actually present in the array. Since this uses the safe interruptible
--   FFI, it allocates a pinned copy of the bytearry if it was not
--   already pinned.
sendByteArray ::
     Fd -- ^ Socket
  -> ByteArray -- ^ Source byte array
  -> CInt -- ^ Offset into source array
  -> CSize -- ^ Length in bytes
  -> SendFlags -- ^ Flags
  -> IO (Either Errno CSize) -- ^ Number of bytes pushed to send buffer
sendByteArray fd b@(ByteArray b#) off len flags = if PM.isByteArrayPinned b
  then errorsFromSize =<< c_safe_bytearray_send fd b# off len flags
  else do
    x@(MutableByteArray x#) <- PM.newPinnedByteArray (csizeToInt len)
    PM.copyByteArray x (cintToInt off) b 0 (csizeToInt len)
    errorsFromSize =<< c_safe_mutablebytearray_no_offset_send fd x# len flags

-- | Send data from a mutable byte array over a network socket. Users
--   may specify an offset and a length to send fewer bytes than are
--   actually present in the array. Since this uses the safe interruptible
--   FFI, it allocates a pinned copy of the bytearry if it was not
--   already pinned.
sendMutableByteArray ::
     Fd -- ^ Socket
  -> MutableByteArray RealWorld -- ^ Source byte array
  -> CInt -- ^ Offset into source array
  -> CSize -- ^ Length in bytes
  -> SendFlags -- ^ Flags
  -> IO (Either Errno CSize) -- ^ Number of bytes pushed to send buffer
sendMutableByteArray fd b@(MutableByteArray b#) off len flags = if PM.isMutableByteArrayPinned b
  then errorsFromSize =<< c_safe_mutablebytearray_send fd b# off len flags
  else do
    x@(MutableByteArray x#) <- PM.newPinnedByteArray (csizeToInt len)
    PM.copyMutableByteArray x (cintToInt off) b 0 (csizeToInt len)
    errorsFromSize =<< c_safe_mutablebytearray_no_offset_send fd x# len flags

-- | Send data from an address over a network socket. This is not guaranteed
--   to send the entire length. This uses the safe interruptible FFI since
--   it may block indefinitely.
send ::
     Fd -- ^ Connected socket
  -> Addr -- ^ Source address
  -> CSize -- ^ Length in bytes
  -> SendFlags -- ^ Flags
  -> IO (Either Errno CSize) -- ^ Number of bytes pushed to send buffer
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
  -> IO (Either Errno CSize) -- ^ Number of bytes pushed to send buffer
unsafeSend fd (Addr addr) len flags =
  c_unsafe_addr_send fd addr len flags >>= errorsFromSize

-- | Send data from a byte array over a network socket. This uses the unsafe FFI;
--   considerations pertaining to 'sendUnsafe' apply to this function as well. Users
--   may specify a length to send fewer bytes than are actually present in the
--   array. However, there is currently no option to provide an offset.
unsafeSendByteArray ::
     Fd -- ^ Socket
  -> ByteArray -- ^ Source byte array
  -> CInt -- ^ Offset into source array
  -> CSize -- ^ Length in bytes
  -> SendFlags -- ^ Flags
  -> IO (Either Errno CSize) -- ^ Number of bytes pushed to send buffer
unsafeSendByteArray fd (ByteArray b) off len flags =
  c_unsafe_bytearray_send fd b off len flags >>= errorsFromSize

-- | Send data from a mutable byte array over a network socket. This uses the unsafe FFI;
--   considerations pertaining to 'sendUnsafe' apply to this function as well. Users
--   may specify a length to send fewer bytes than are actually present in the
--   array.
unsafeSendMutableByteArray ::
     Fd -- ^ Socket
  -> MutableByteArray RealWorld -- ^ Source mutable byte array
  -> CInt -- ^ Offset into source array
  -> CSize -- ^ Length in bytes
  -> SendFlags -- ^ Flags
  -> IO (Either Errno CSize) -- ^ Number of bytes pushed to send buffer
unsafeSendMutableByteArray fd (MutableByteArray b) off len flags =
  c_unsafe_mutable_bytearray_send fd b off len flags >>= errorsFromSize

-- | Receive data into an address from a network socket. This wraps @recv@ using
--   the safe interruptible FFI. When the returned size is zero, there are no
--   additional bytes to receive and the peer has performed an orderly shutdown.
receive ::
     Fd -- ^ Socket
  -> Addr -- ^ Source address
  -> CSize -- ^ Length in bytes
  -> ReceiveFlags -- ^ Flags
  -> IO (Either Errno CSize)
receive fd (Addr addr) len flags =
  c_safe_addr_recv fd addr len flags >>= errorsFromSize

-- | Receive data into a byte array from a network socket. This wraps @recv@ using
--   the safe interruptible FFI. When the returned size is zero, there are no
--   additional bytes to receive and the peer has performed an orderly shutdown.
receiveByteArray ::
     Fd -- ^ Socket
  -> CSize -- ^ Length in bytes
  -> ReceiveFlags -- ^ Flags
  -> IO (Either Errno ByteArray)
receiveByteArray fd len flags = do
  m <- PM.newPinnedByteArray (csizeToInt len)
  let !(Addr addr) = PM.mutableByteArrayContents m
  r <- c_safe_addr_recv fd addr len flags
  if r /= (-1)
    then do
      -- Why copy when we could just shrink? We want to always return
      -- byte arrays that are not explicitly pinned.
      let sz = cssizeToInt r
      x <- PM.newByteArray sz
      PM.copyMutableByteArray x 0 m 0 sz
      a <- PM.unsafeFreezeByteArray x
      pure (Right a)
    else fmap Left getErrno

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
--   actually present in the mutable byte array.
unsafeReceiveMutableByteArray ::
     Fd -- ^ Socket
  -> MutableByteArray RealWorld -- ^ Destination byte array
  -> CInt -- ^ Destination offset
  -> CSize -- ^ Maximum bytes to receive
  -> ReceiveFlags -- ^ Flags
  -> IO (Either Errno CSize) -- ^ Bytes received into array
unsafeReceiveMutableByteArray fd (MutableByteArray b) off len flags =
  c_unsafe_mutable_byte_array_recv fd b off len flags >>= errorsFromSize

-- | Close a socket. The <http://pubs.opengroup.org/onlinepubs/009696899/functions/close.html POSIX specification>
--   includes more details. This uses the safe interruptible FFI.
close ::
     Fd -- ^ Socket
  -> IO (Either Errno ())
close fd = c_safe_close fd >>= errorsFromInt

-- | Close a socket. This uses the unsafe FFI. According to the
--   <http://pubs.opengroup.org/onlinepubs/009696899/functions/close.html POSIX specification>,
--   "If @fildes@ refers to a socket, @close()@ shall cause the socket to
--   be destroyed. If the socket is in connection-mode, and the @SO_LINGER@
--   option is set for the socket with non-zero linger time, and the socket
--   has untransmitted data, then @close()@ shall block for up to the current
--   linger interval until all data is transmitted."
unsafeClose ::
     Fd -- ^ Socket
  -> IO (Either Errno ())
unsafeClose fd = c_unsafe_close fd >>= errorsFromInt

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

csizeToInt :: CSize -> Int
csizeToInt = fromIntegral

cssizeToInt :: CSsize -> Int
cssizeToInt = fromIntegral

-- only call this when it is known that the argument is non-negative
cssizeToCSize :: CSsize -> CSize
cssizeToCSize = fromIntegral

-- touchByteArray :: ByteArray -> IO ()
-- touchByteArray (ByteArray x) = touchByteArray# x
-- 
-- touchByteArray# :: ByteArray# -> IO ()
-- touchByteArray# x = IO $ \s -> case touch# x s of s' -> (# s', () #)

