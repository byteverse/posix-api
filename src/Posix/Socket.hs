{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnliftedFFITypes #-}

{- | Types and functions related to the POSIX sockets API.
  Unusual characteristics:

  * Any time the standard calls for @socklen_t@, we use
    @CInt@ instead. Linus Torvalds <https://yarchive.net/comp/linux/socklen_t.html writes>
    that \"Any sane library must have @socklen_t@ be the same size as @int@.
    Anything else breaks any BSD socket layer stuff.\"
  * Send and receive each have several variants. They are distinguished by
    the safe\/unsafe FFI use and by the @Addr@\/@ByteArray@/@MutableByteArray@
    buffer type. They all call @send@ or @recv@ exactly once. They do not
    repeatedly make syscalls like some of the functions in @network@.
    Users who want that behavior need to build on top of this package.
  * There are no requirements on the pinnedness of @ByteArray@ arguments
    passed to any of these functions. If wrappers of the safe FFI are
    passed unpinned @ByteArray@ arguments, they will copy the contents
    into pinned memory before invoking the foreign function.
-}
module Posix.Socket
  ( -- * Functions

    -- ** Socket
    uninterruptibleSocket
  , socket
  , withSocket

    -- ** Socket Pair
  , uninterruptibleSocketPair

    -- ** Address Resolution
  , getAddressInfo
  , uninterruptibleFreeAddressInfo

    -- ** Bind
  , uninterruptibleBind

    -- ** Connect
  , connect
  , uninterruptibleConnect
  , uninterruptibleConnectPtr

    -- ** Listen
  , uninterruptibleListen

    -- ** Accept
  , accept
  , uninterruptibleAccept
  , accept_

    -- ** Get Socket Name
  , uninterruptibleGetSocketName

    -- ** Get Socket Option
  , uninterruptibleGetSocketOption

    -- ** Set Socket Option
  , uninterruptibleSetSocketOption
  , uninterruptibleSetSocketOptionByteArray
  , uninterruptibleSetSocketOptionInt

    -- ** Close
  , F.close
  , F.uninterruptibleClose
  , F.uninterruptibleErrorlessClose

    -- ** Shutdown
  , uninterruptibleShutdown

    -- ** Send
  , send
  , sendByteArray
  , sendMutableByteArray
  , uninterruptibleSend
  , uninterruptibleSendByteArray
  , uninterruptibleSendMutableByteArray

    -- ** Send To
  , uninterruptibleSendToByteArray
  , uninterruptibleSendToMutableByteArray
  , uninterruptibleSendToInternet
  , uninterruptibleSendToInternetByteArray
  , uninterruptibleSendToInternetMutableByteArray

    -- ** Write Vector

    -- ** Receive
  , receive
  , receiveByteArray
  , uninterruptibleReceive
  , uninterruptibleReceiveMutableByteArray

    -- ** Receive From
  , uninterruptibleReceiveFromMutableByteArray
  , uninterruptibleReceiveFromMutableByteArray_
  , uninterruptibleReceiveFrom_
  , uninterruptibleReceiveFromInternet
  , uninterruptibleReceiveFromInternetMutableByteArray

    -- ** Receive Message
    -- $receiveMessage
  , uninterruptibleSendMessageA
  , uninterruptibleSendMessageB

    -- ** Byte-Order Conversion
    -- $conversion
  , hostToNetworkLong
  , hostToNetworkShort
  , networkToHostLong
  , networkToHostShort

    -- * Types
  , Family (..)
  , Type (..)
  , Protocol (..)
  , OptionName (..)
  , OptionValue (..)
  , Level (..)
  , Message (..)
  , MessageFlags (..)
  , ShutdownType (..)
  , AddressInfo

    -- * Socket Address

    -- ** Types
  , SocketAddress (..)
  , PST.SocketAddressInternet (..)
  , PST.SocketAddressUnix (..)

    -- ** Encoding
  , PSP.encodeSocketAddressInternet
  , PSP.encodeSocketAddressUnix

    -- ** Decoding
  , PSP.decodeSocketAddressInternet
  , PSP.indexSocketAddressInternet

    -- ** Sizes
  , PSP.sizeofSocketAddressInternet

    -- * Data Construction

    -- ** Socket Domains
  , pattern PST.Unix
  , pattern PST.Unspecified
  , pattern PST.Internet
  , pattern PST.Internet6

    -- ** Socket Types
  , PST.stream
  , PST.datagram
  , PST.raw
  , PST.sequencedPacket

    -- ** Protocols
  , PST.defaultProtocol
  , PST.rawProtocol
  , PST.icmp
  , PST.tcp
  , PST.udp
  , PST.ip
  , PST.ipv6

    -- ** Receive Flags
  , PST.peek
  , PST.outOfBand
  , PST.waitAll

    -- ** Send Flags
  , PST.noSignal

    -- ** Shutdown Types
  , PST.read
  , PST.write
  , PST.readWrite

    -- ** Socket Levels
  , PST.levelSocket

    -- ** Option Names
  , PST.optionError
  , PST.bindToDevice
  , PST.broadcast
  , PST.reuseAddress

    -- ** Address Info

    -- *** Peek
  , PST.peekAddressInfoFlags

    -- *** Poke
  , PST.pokeAddressInfoFlags

    -- *** Metadata
  , PST.sizeofAddressInfo

    -- ** Message Header

    -- *** Peek
  , PST.peekMessageHeaderName
  , PST.peekMessageHeaderNameLength
  , PST.peekMessageHeaderIOVector
  , PST.peekMessageHeaderIOVectorLength
  , PST.peekMessageHeaderControl
  , PST.peekMessageHeaderControlLength
  , PST.peekMessageHeaderFlags
  , PST.peekControlMessageHeaderLevel
  , PST.peekControlMessageHeaderLength
  , PST.peekControlMessageHeaderType

    -- *** Poke
  , PST.pokeMessageHeaderName
  , PST.pokeMessageHeaderNameLength
  , PST.pokeMessageHeaderIOVector
  , PST.pokeMessageHeaderIOVectorLength
  , PST.pokeMessageHeaderControl
  , PST.pokeMessageHeaderControlLength
  , PST.pokeMessageHeaderFlags

    -- *** Metadata
  , PST.sizeofMessageHeader

    -- ** IO Vector

    -- *** Peek
  , PST.peekIOVectorBase
  , PST.peekIOVectorLength

    -- *** Poke
  , PST.pokeIOVectorBase
  , PST.pokeIOVectorLength

    -- *** Metadata
  , PST.sizeofIOVector
  ) where

import Control.Exception (mask, onException)
import Data.Primitive (ByteArray (..), MutableByteArray (..), MutablePrimArray (..))
import Data.Primitive.Addr (Addr (..))
import Data.Primitive.ByteArray.Offset (MutableByteArrayOffset (..))
import Data.Primitive.PrimArray.Offset (MutablePrimArrayOffset (..))
import Data.Void (Void)
import Data.Word (Word16, Word32, Word8, byteSwap16, byteSwap32)
import Foreign.C.Error (Errno (Errno), getErrno)
import Foreign.C.String (CString)
import Foreign.C.Types (CInt (..), CSize (..))
import Foreign.Ptr (nullPtr)
import GHC.ByteOrder (ByteOrder (BigEndian, LittleEndian), targetByteOrder)
import GHC.Exts (Addr#, ByteArray#, Int (I#), MutableByteArray#, Ptr (Ptr), RealWorld, shrinkMutableByteArray#)
import Posix.Socket.Types
  ( AddressInfo
  , Family (..)
  , Level (..)
  , Message (..)
  , MessageFlags (..)
  , OptionName (..)
  , OptionValue (..)
  , Protocol (..)
  , ShutdownType (..)
  , SocketAddress (..)
  , SocketAddressInternet (..)
  , Type (..)
  )
import System.Posix.Types (CSsize (..), Fd (..))

import qualified Control.Monad.Primitive as PM
import qualified Data.Primitive as PM
import qualified GHC.Exts as Exts
import qualified Posix.File as F
import qualified Posix.Socket.Types as PST

-- This module include operating-system specific code used
-- to serialize some of various kind of socket address types.
import qualified Posix.Socket.Platform as PSP

-- getaddrinfo cannot use the unsafe ffi
foreign import ccall safe "sys/socket.h getaddrinfo"
  c_safe_getaddrinfo ::
    CString ->
    CString ->
    Ptr AddressInfo ->
    MutableByteArray# RealWorld -> -- actually a `Ptr (Ptr AddressInfo))`.
    IO Errno

-- | Free the @addrinfo@ at the pointer.
foreign import ccall safe "sys/socket.h freeaddrinfo"
  uninterruptibleFreeAddressInfo :: Ptr AddressInfo -> IO ()

foreign import ccall unsafe "sys/socket.h socket"
  c_socket :: Family -> Type -> Protocol -> IO Fd

foreign import ccall unsafe "sys/socket.h socketpair"
  c_socketpair :: Family -> Type -> Protocol -> MutableByteArray# RealWorld -> IO CInt

foreign import ccall unsafe "sys/socket.h listen"
  c_listen :: Fd -> CInt -> IO CInt

foreign import ccall unsafe "unistd.h shutdown"
  c_unsafe_shutdown :: Fd -> ShutdownType -> IO CInt

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
foreign import ccall safe "sys/socket.h accept"
  c_safe_accept ::
    Fd ->
    MutableByteArray# RealWorld -> -- SocketAddress
    MutableByteArray# RealWorld -> -- Ptr CInt
    IO Fd
foreign import ccall unsafe "sys/socket.h accept"
  c_unsafe_accept ::
    Fd ->
    MutableByteArray# RealWorld -> -- SocketAddress
    MutableByteArray# RealWorld -> -- Ptr CInt
    IO Fd

-- This variant of accept is used when we do not care about the
-- remote sockaddr. We pass null.
foreign import ccall safe "sys/socket.h accept"
  c_safe_ptr_accept :: Fd -> Ptr Void -> Ptr CInt -> IO Fd

foreign import ccall unsafe "sys/socket.h getsockname"
  c_unsafe_getsockname ::
    Fd ->
    MutableByteArray# RealWorld -> -- SocketAddress
    MutableByteArray# RealWorld -> -- Addr length (Ptr CInt)
    IO CInt

foreign import ccall unsafe "sys/socket.h getsockopt"
  c_unsafe_getsockopt ::
    Fd ->
    Level ->
    OptionName ->
    MutableByteArray# RealWorld -> -- Option value
    MutableByteArray# RealWorld -> -- Option len (Ptr CInt)
    IO CInt

foreign import ccall unsafe "sys/socket.h setsockopt_int"
  c_unsafe_setsockopt_int ::
    Fd ->
    Level ->
    OptionName ->
    CInt -> -- option_value
    IO CInt

foreign import ccall unsafe "sys/socket.h setsockopt"
  c_unsafe_setsockopt ::
    Fd ->
    Level ->
    OptionName ->
    Ptr Void -> -- option_val
    CInt -> -- option_len
    IO CInt

foreign import ccall unsafe "sys/socket.h setsockopt"
  c_unsafe_setsockopt_ba ::
    Fd ->
    Level ->
    OptionName ->
    ByteArray# -> -- option_val
    CInt -> -- option_len
    IO CInt

-- Per the spec the type signature of connect is:
--   int connect(int sockfd, const struct sockaddr *addr, socklen_t addrlen);
-- The bytearray argument is actually SocketAddress.
foreign import ccall safe "sys/socket.h connect"
  c_safe_connect :: Fd -> ByteArray# -> CInt -> IO CInt
foreign import ccall safe "sys/socket.h connect"
  c_safe_mutablebytearray_connect :: Fd -> MutableByteArray# RealWorld -> CInt -> IO CInt
foreign import ccall unsafe "sys/socket.h connect"
  c_unsafe_connect :: Fd -> ByteArray# -> CInt -> IO CInt
foreign import ccall unsafe "sys/socket.h connect"
  c_unsafe_connect_addr :: Fd -> Addr# -> CInt -> IO CInt

-- There are several options for wrapping send. Both safe and unsafe
-- are useful. Additionally, in the unsafe category, we also
-- have the option of writing to either an address or a byte array.
-- Unsafe FFI calls guarantee that byte arrays will not be relocated
-- while the FFI call is taking place. Safe FFI calls do not have
-- this guarantee, so internally we must be careful when using these to only
-- provide pinned byte arrays as arguments.
foreign import ccall safe "sys/socket.h send"
  c_safe_addr_send :: Fd -> Addr# -> CSize -> MessageFlags 'Send -> IO CSsize
foreign import ccall safe "sys/socket.h send_offset"
  c_safe_bytearray_send :: Fd -> ByteArray# -> Int -> CSize -> MessageFlags 'Send -> IO CSsize
foreign import ccall safe "sys/socket.h send_offset"
  c_safe_mutablebytearray_send :: Fd -> MutableByteArray# RealWorld -> Int -> CSize -> MessageFlags 'Send -> IO CSsize
foreign import ccall safe "sys/socket.h send"
  c_safe_mutablebytearray_no_offset_send :: Fd -> MutableByteArray# RealWorld -> CSize -> MessageFlags 'Send -> IO CSsize
foreign import ccall unsafe "sys/socket.h send"
  c_unsafe_addr_send :: Fd -> Addr# -> CSize -> MessageFlags 'Send -> IO CSsize
foreign import ccall unsafe "sys/socket.h send_offset"
  c_unsafe_bytearray_send :: Fd -> ByteArray# -> Int -> CSize -> MessageFlags 'Send -> IO CSsize
foreign import ccall unsafe "sys/socket.h send_offset"
  c_unsafe_mutable_bytearray_send :: Fd -> MutableByteArray# RealWorld -> Int -> CSize -> MessageFlags 'Send -> IO CSsize

-- The ByteArray# (second to last argument) is a SocketAddress.
foreign import ccall unsafe "sys/socket.h sendto_offset"
  c_unsafe_bytearray_sendto :: Fd -> ByteArray# -> Int -> CSize -> MessageFlags 'Send -> ByteArray# -> CInt -> IO CSsize

-- The ByteArray# (second to last argument) is a SocketAddress.
foreign import ccall unsafe "sys/socket.h sendto_offset"
  c_unsafe_mutable_bytearray_sendto :: Fd -> MutableByteArray# RealWorld -> Int -> CSize -> MessageFlags 'Send -> ByteArray# -> CInt -> IO CSsize
foreign import ccall unsafe "sys/socket.h sendto_inet_offset"
  c_unsafe_mutable_bytearray_sendto_inet :: Fd -> MutableByteArray# RealWorld -> Int -> CSize -> MessageFlags 'Send -> Word16 -> Word32 -> IO CSsize
foreign import ccall unsafe "HaskellPosix.h sendto_inet_offset"
  c_unsafe_bytearray_sendto_inet :: Fd -> ByteArray# -> Int -> CSize -> MessageFlags 'Send -> Word16 -> Word32 -> IO CSsize
foreign import ccall unsafe "HaskellPosix.h sendto_inet_addr"
  c_unsafe_addr_sendto_inet :: Fd -> Addr# -> CSize -> MessageFlags 'Send -> Word16 -> Word32 -> IO CSsize

foreign import ccall unsafe "HaskellPosix.h sendmsg_a"
  c_unsafe_sendmsg_a :: Fd -> Addr# -> CSize -> MutableByteArray# RealWorld -> Int -> CSize -> MessageFlags 'Send -> IO CSsize

foreign import ccall unsafe "HaskellPosix.h sendmsg_b"
  c_unsafe_sendmsg_b :: Fd -> MutableByteArray# RealWorld -> Int -> CSize -> Addr# -> CSize -> MessageFlags 'Send -> IO CSsize

-- There are several ways to wrap recv.
foreign import ccall safe "sys/socket.h recv"
  c_safe_addr_recv :: Fd -> Addr# -> CSize -> MessageFlags 'Receive -> IO CSsize
foreign import ccall unsafe "sys/socket.h recv"
  c_unsafe_addr_recv :: Fd -> Addr# -> CSize -> MessageFlags 'Receive -> IO CSsize
foreign import ccall unsafe "sys/socket.h recv_offset"
  c_unsafe_mutable_byte_array_recv :: Fd -> MutableByteArray# RealWorld -> Int -> CSize -> MessageFlags 'Receive -> IO CSsize

-- The last two arguments are SocketAddress and Ptr CInt.
foreign import ccall unsafe "sys/socket.h recvfrom_offset"
  c_unsafe_mutable_byte_array_recvfrom :: Fd -> MutableByteArray# RealWorld -> Int -> CSize -> MessageFlags 'Receive -> MutableByteArray# RealWorld -> MutableByteArray# RealWorld -> IO CSsize
foreign import ccall unsafe "sys/socket.h recvfrom_offset_peerless"
  c_unsafe_mutable_byte_array_peerless_recvfrom ::
    Fd ->
    MutableByteArray# RealWorld ->
    Int ->
    CSize ->
    MessageFlags 'Receive ->
    IO CSsize
foreign import ccall unsafe "sys/socket.h recvfrom_addr_peerless"
  c_unsafe_addr_peerless_recvfrom ::
    Fd -> Addr# -> CSize -> MessageFlags 'Receive -> IO CSsize
foreign import ccall unsafe "sys/socket.h recvfrom_offset_inet"
  c_unsafe_recvfrom_inet ::
    Fd ->
    MutableByteArray# RealWorld ->
    Int ->
    CSize ->
    MessageFlags 'Receive ->
    MutableByteArray# RealWorld ->
    Int ->
    IO CSsize
foreign import ccall unsafe "sys/socket.h recvfrom_offset_inet_addr"
  c_unsafe_recvfrom_inet_addr ::
    Fd ->
    Addr# ->
    CSize ->
    MessageFlags 'Receive ->
    MutableByteArray# RealWorld ->
    Int ->
    IO CSsize

{- | Create an endpoint for communication, returning a file
  descriptor that refers to that endpoint. The
  <http://pubs.opengroup.org/onlinepubs/9699919799/functions/socket.html POSIX specification>
  includes more details. No special preparation is required before calling
  this function. The author believes that it cannot block for a prolonged
  period of time.
-}
uninterruptibleSocket ::
  -- | Communications domain (e.g. 'internet', 'unix')
  Family ->
  -- | Socket type (e.g. 'datagram', 'stream') with flags
  Type ->
  -- | Protocol
  Protocol ->
  IO (Either Errno Fd)
uninterruptibleSocket dom typ prot = c_socket dom typ prot >>= errorsFromFd

-- | Alias for 'uninterruptibleSocket'.
socket ::
  -- | Communications domain (e.g. 'internet', 'unix')
  Family ->
  -- | Socket type (e.g. 'datagram', 'stream') with flags
  Type ->
  -- | Protocol
  Protocol ->
  IO (Either Errno Fd)
socket = uninterruptibleSocket

{- | Helper function for the common case where 'socket' or
'uninterruptibleSocket' is paired with 'close'. This ensures that the
socket is closed even in the case of an exception. Do not call 'close' in
the callback since 'close' is called by this function after the callback
completes (or after an exception is thrown).

This is implementated with @mask@ (and restore) and @onException@
directly rather than with @bracket@.
-}
withSocket ::
  -- | Communications domain (e.g. 'internet', 'unix')
  Family ->
  -- | Socket type (e.g. 'datagram', 'stream') with flags
  Type ->
  -- | Protocol
  Protocol ->
  -- | Callback that uses the socket. Must not close the socket.
  -- The callback is not used when the @socket()@ call fails.
  (Fd -> IO a) ->
  IO (Either Errno a)
{-# INLINE withSocket #-}
withSocket !dom !typ !prot cb =
  mask $ \restore -> do
    r <- c_socket dom typ prot
    if r > (-1)
      then do
        a <- restore (cb r) `onException` F.close r
        _ <- F.close r
        pure (Right a)
      else fmap Left getErrno

{- | Create an unbound pair of connected sockets in a specified domain, of
  a specified type, under the protocol optionally specified by the protocol
  argument. The <http://pubs.opengroup.org/onlinepubs/9699919799/functions/socketpair.html POSIX specification>
  includes more details. No special preparation is required before calling
  this function. The author believes that it cannot block for a prolonged
  period of time.
-}
uninterruptibleSocketPair ::
  -- | Communications domain (probably 'unix')
  Family ->
  -- | Socket type (e.g. 'datagram', 'stream') with flags
  Type ->
  -- | Protocol
  Protocol ->
  IO (Either Errno (Fd, Fd))
uninterruptibleSocketPair dom typ prot = do
  -- If this ever switches to the safe FFI, we will need to use
  -- a pinned array here instead.
  (sockets@(MutablePrimArray sockets#) :: MutablePrimArray RealWorld Fd) <- PM.newPrimArray 2
  r <- c_socketpair dom typ prot sockets#
  if r == 0
    then do
      fd1 <- PM.readPrimArray sockets 0
      fd2 <- PM.readPrimArray sockets 1
      pure (Right (fd1, fd2))
    else fmap Left getErrno

{- | Given node and service, which identify an Internet host and a service,
@getaddrinfo()@ returns one or more @addrinfo@ structures. The type of this
wrapper differs slightly from the type of its C counterpart. Remember to call
'uninterruptibleFreeAddressInfo' when finished with the result.
-}
getAddressInfo ::
  -- | Node, identifies an Internet host
  CString ->
  -- | Service
  CString ->
  -- | Hints
  Ptr AddressInfo ->
  IO (Either Errno (Ptr AddressInfo))
getAddressInfo !node !service !hints = do
  resBuf@(MutableByteArray resBuf#) <- PM.newPinnedByteArray (PM.sizeOf (undefined :: Ptr ()))
  c_safe_getaddrinfo node service hints resBuf# >>= \case
    Errno 0 -> do
      res <- PM.readByteArray resBuf 0
      pure (Right res)
    e -> pure (Left e)

{- | Assign a local socket address address to a socket identified by
  descriptor socket that has no local socket address assigned. The
  <http://pubs.opengroup.org/onlinepubs/9699919799/functions/bind.html POSIX specification>
  includes more details. The 'SocketAddress' represents the @sockaddr@ pointer argument, together
  with its @socklen_t@ size, as a byte array. This allows @bind@ to
  be used with @sockaddr@ extensions on various platforms. No special
  preparation is required before calling this function. The author
  believes that it cannot block for a prolonged period of time.
-}
uninterruptibleBind ::
  -- | Socket
  Fd ->
  -- | Socket address, extensible tagged union
  SocketAddress ->
  IO (Either Errno ())
uninterruptibleBind fd (SocketAddress b@(ByteArray b#)) =
  c_bind fd b# (intToCInt (PM.sizeofByteArray b)) >>= errorsFromInt_

{- | Mark the socket as a passive socket, that is, as a socket that
  will be used to accept incoming connection requests using @accept@.
  The <http://pubs.opengroup.org/onlinepubs/9699919799/functions/listen.html POSIX specification>
  includes more details. No special preparation is required before
  calling this function. The author believes that it cannot block
  for a prolonged period of time.
-}
uninterruptibleListen ::
  -- | Socket
  Fd ->
  -- | Backlog
  CInt ->
  IO (Either Errno ())
uninterruptibleListen fd backlog = c_listen fd backlog >>= errorsFromInt_

{- | Connect the socket to the specified socket address.
  The <http://pubs.opengroup.org/onlinepubs/9699919799/functions/connect.html POSIX specification>
  includes more details.
-}
connect ::
  -- | Fd
  Fd ->
  -- | Socket address, extensible tagged union
  SocketAddress ->
  IO (Either Errno ())
connect fd (SocketAddress sockAddr@(ByteArray sockAddr#)) =
  case isByteArrayPinned sockAddr of
    True -> c_safe_connect fd sockAddr# (intToCInt (PM.sizeofByteArray sockAddr)) >>= errorsFromInt_
    False -> do
      let len = PM.sizeofByteArray sockAddr
      x@(MutableByteArray x#) <- PM.newPinnedByteArray len
      PM.copyByteArray x 0 sockAddr 0 len
      c_safe_mutablebytearray_connect fd x# (intToCInt len) >>= errorsFromInt_

{- | Connect the socket to the specified socket address.
  The <http://pubs.opengroup.org/onlinepubs/9699919799/functions/connect.html POSIX specification>
  includes more details. The only sensible way to use this is to
  give a nonblocking socket as the argument.
-}
uninterruptibleConnect ::
  -- | Fd
  Fd ->
  -- | Socket address, extensible tagged union
  SocketAddress ->
  IO (Either Errno ())
uninterruptibleConnect fd (SocketAddress sockAddr@(ByteArray sockAddr#)) =
  c_unsafe_connect fd sockAddr# (intToCInt (PM.sizeofByteArray sockAddr)) >>= errorsFromInt_

uninterruptibleConnectPtr ::
  -- | Fd
  Fd ->
  -- | Socket address
  Ptr a ->
  -- | Size of socket address
  Int ->
  IO (Either Errno ())
uninterruptibleConnectPtr !fd (Ptr sockAddr#) !sz =
  c_unsafe_connect_addr fd sockAddr# (intToCInt sz) >>= errorsFromInt_

{- | Extract the first connection on the queue of pending connections. The
  <http://pubs.opengroup.org/onlinepubs/9699919799/functions/accept.html POSIX specification>
  includes more details. This function\'s type differs slightly from
  the specification:

  > int accept(int socket, struct sockaddr *restrict address, socklen_t *restrict address_len);

  Instead of requiring the caller to prepare buffers through which
  information is returned, this haskell binding to @accept@ prepares
  those buffers internally. This eschews C\'s characteristic buffer-passing
  in favor of the Haskell convention of allocating internally and returning.

  More specifically, this binding lacks an argument corresponding to the
  @sockaddr@ buffer from the specification. That mutable buffer is allocated
  internally, resized and frozen upon a success, and returned along with
  the file descriptor of the accepted socket. The size of this buffer is
  determined by the second argument (maximum socket address size). This
  size argument is also writen to the @address_len@ buffer, which is also
  allocated internally. The size returned through this pointer is used to
  resize the @sockaddr@ buffer, which is then frozen so that an immutable
  'SocketAddress' is returned to the end user.

  For applications uninterested in the peer (described by @sockaddr@),
  POSIX @accept@ allows the null pointer to be passed as both @address@ and
  @address_len@. This behavior is provided by 'accept_'.
-}
accept ::
  -- | Listening socket
  Fd ->
  -- | Maximum socket address size
  CInt ->
  -- | Peer information and connected socket
  IO (Either Errno (CInt, SocketAddress, Fd))
accept !sock !maxSz = do
  sockAddrBuf@(MutableByteArray sockAddrBuf#) <- PM.newPinnedByteArray (cintToInt maxSz)
  lenBuf@(MutableByteArray lenBuf#) <- PM.newPinnedByteArray (PM.sizeOf (undefined :: CInt))
  PM.writeByteArray lenBuf 0 maxSz
  r <- c_safe_accept sock sockAddrBuf# lenBuf#
  if r > (-1)
    then do
      (sz :: CInt) <- PM.readByteArray lenBuf 0
      -- Why copy when we could just shrink? We want to always return
      -- byte arrays that are not explicitly pinned.
      let minSz = min sz maxSz
      x <- PM.newByteArray (cintToInt minSz)
      PM.copyMutableByteArray x 0 sockAddrBuf 0 (cintToInt minSz)
      sockAddr <- PM.unsafeFreezeByteArray x
      pure (Right (sz, SocketAddress sockAddr, r))
    else fmap Left getErrno

{- | See 'accept'. This uses the unsafe FFI. Consequently, it does not
  not need to allocate pinned memory. It only makes sense to call this
  on a nonblocking socket.
-}
uninterruptibleAccept ::
  -- | Listening socket
  Fd ->
  -- | Maximum socket address size
  CInt ->
  -- | Peer information and connected socket
  IO (Either Errno (CInt, SocketAddress, Fd))
uninterruptibleAccept !sock !maxSz = do
  sockAddrBuf@(MutableByteArray sockAddrBuf#) <- PM.newByteArray (cintToInt maxSz)
  lenBuf@(MutableByteArray lenBuf#) <- PM.newByteArray (PM.sizeOf (undefined :: CInt))
  PM.writeByteArray lenBuf 0 maxSz
  r <- c_unsafe_accept sock sockAddrBuf# lenBuf#
  if r > (-1)
    then do
      (sz :: CInt) <- PM.readByteArray lenBuf 0
      if sz < maxSz
        then shrinkMutableByteArray sockAddrBuf (cintToInt sz)
        else pure ()
      sockAddr <- PM.unsafeFreezeByteArray sockAddrBuf
      pure (Right (sz, SocketAddress sockAddr, r))
    else fmap Left getErrno

{- | A variant of 'accept' that does not provide the user with a
  'SocketAddress' detailing the peer.
-}
accept_ ::
  -- | Listening socket
  Fd ->
  -- | Connected socket
  IO (Either Errno Fd)
accept_ sock =
  c_safe_ptr_accept sock nullPtr nullPtr >>= errorsFromFd

{- | Retrieve the locally-bound name of the specified socket. The
  <http://pubs.opengroup.org/onlinepubs/9699919799/functions/accept.html POSIX specification>
  of @getsockname@ includes more details.
-}
uninterruptibleGetSocketName ::
  -- | Socket
  Fd ->
  -- | Maximum socket address size
  CInt ->
  IO (Either Errno (CInt, SocketAddress))
uninterruptibleGetSocketName sock maxSz = do
  sockAddrBuf@(MutableByteArray sockAddrBuf#) <- PM.newByteArray (cintToInt maxSz)
  lenBuf@(MutableByteArray lenBuf#) <- PM.newByteArray (PM.sizeOf (undefined :: CInt))
  PM.writeByteArray lenBuf 0 maxSz
  r <- c_unsafe_getsockname sock sockAddrBuf# lenBuf#
  if r == 0
    then do
      (sz :: CInt) <- PM.readByteArray lenBuf 0
      if sz < maxSz
        then shrinkMutableByteArray sockAddrBuf (cintToInt sz)
        else pure ()
      sockAddr <- PM.unsafeFreezeByteArray sockAddrBuf
      pure (Right (sz, SocketAddress sockAddr))
    else fmap Left getErrno

{- | Retrieve the value for the option specified by the 'Option' argument for
  the socket specified by the 'Fd' argument. The
  <http://pubs.opengroup.org/onlinepubs/9699919799/functions/getsockopt.html POSIX specification>
  of @getsockopt@ includes more details.
-}
uninterruptibleGetSocketOption ::
  -- | Socket
  Fd ->
  -- | Socket level
  Level ->
  OptionName -> -- Option name

  -- | Maximum option value size
  CInt ->
  IO (Either Errno (CInt, OptionValue))
uninterruptibleGetSocketOption sock level optName maxSz = do
  valueBuf@(MutableByteArray valueBuf#) <- PM.newByteArray (cintToInt maxSz)
  lenBuf@(MutableByteArray lenBuf#) <- PM.newByteArray (PM.sizeOf (undefined :: CInt))
  PM.writeByteArray lenBuf 0 maxSz
  r <- c_unsafe_getsockopt sock level optName valueBuf# lenBuf#
  if r == 0
    then do
      (sz :: CInt) <- PM.readByteArray lenBuf 0
      if sz < maxSz
        then shrinkMutableByteArray valueBuf (cintToInt sz)
        else pure ()
      value <- PM.unsafeFreezeByteArray valueBuf
      pure (Right (sz, OptionValue value))
    else fmap Left getErrno

{- | Set the value for the option specified by the 'Option' argument for
  the socket specified by the 'Fd' argument. The
  <http://pubs.opengroup.org/onlinepubs/9699919799/functions/getsockopt.html POSIX specification>
  of @getsockopt@ includes more details. This variant requires that the
  size of the @option_value@
  be the same as the size of 'CInt'. That is, the @option_name@ must
  describe an option that is represented by a C integer. This is a
  common case, so we avoid allocations by reference-passing in C.
-}
uninterruptibleSetSocketOptionInt ::
  -- | Socket
  Fd ->
  -- | Socket level
  Level ->
  -- | Option name
  OptionName ->
  -- | Option value
  CInt ->
  IO (Either Errno ())
uninterruptibleSetSocketOptionInt sock level optName optValue =
  c_unsafe_setsockopt_int sock level optName optValue >>= errorsFromInt_

{- | Set the value for the option specified by the 'Option' argument for
  the socket specified by the 'Fd' argument. The
  <http://pubs.opengroup.org/onlinepubs/9699919799/functions/getsockopt.html POSIX specification>
  of @getsockopt@ includes more details.
-}
uninterruptibleSetSocketOption ::
  -- | Socket
  Fd ->
  -- | Socket level
  Level ->
  -- | Option name
  OptionName ->
  -- | Option value
  Ptr Void ->
  -- | Option value length
  CInt ->
  IO (Either Errno ())
uninterruptibleSetSocketOption sock level optName optValue optLen =
  c_unsafe_setsockopt sock level optName optValue optLen >>= errorsFromInt_

{- | Variant of 'uninterruptibleSetSocketOption' that accepts the option
  as a byte array instead of a pointer into unmanaged memory. The argument
  does not need to be pinned.
-}
uninterruptibleSetSocketOptionByteArray ::
  -- | Socket
  Fd ->
  -- | Socket level
  Level ->
  -- | Option name
  OptionName ->
  -- | Option value
  ByteArray ->
  -- | Option value length
  CInt ->
  IO (Either Errno ())
uninterruptibleSetSocketOptionByteArray sock level optName (ByteArray optVal) optLen =
  c_unsafe_setsockopt_ba sock level optName optVal optLen >>= errorsFromInt_

{- | Send data from a byte array over a network socket. Users
  may specify an offset and a length to send fewer bytes than are
  actually present in the array. Since this uses the safe
  FFI, it allocates a pinned copy of the bytearry if it was not
  already pinned.
-}
sendByteArray ::
  -- | Socket
  Fd ->
  -- | Source byte array
  ByteArray ->
  -- | Offset into source array
  Int ->
  -- | Length in bytes
  CSize ->
  -- | Flags
  MessageFlags 'Send ->
  -- | Number of bytes pushed to send buffer
  IO (Either Errno CSize)
sendByteArray fd b@(ByteArray b#) off len flags =
  if isByteArrayPinned b
    then errorsFromSize =<< c_safe_bytearray_send fd b# off len flags
    else do
      x@(MutableByteArray x#) <- PM.newPinnedByteArray (csizeToInt len)
      PM.copyByteArray x off b 0 (csizeToInt len)
      errorsFromSize =<< c_safe_mutablebytearray_no_offset_send fd x# len flags

{- | Copy and pin a byte array if, it's not already pinned.
pinByteArray :: ByteArray -> IO (Maybe ByteArray)
{\-# INLINE pinByteArray #-\}
pinByteArray byteArray =
  if isByteArrayPinned byteArray
    then
      pure Nothing
    else do
      pinnedByteArray <- PM.newPinnedByteArray len
      PM.copyByteArray pinnedByteArray 0 byteArray 0 len
      r <- PM.unsafeFreezeByteArray pinnedByteArray
      pure (Just r)
  where
    len = PM.sizeofByteArray byteArray
-}

{- | Send two payloads (one from unmanaged memory and one from
managed memory) over a network socket.
-}
uninterruptibleSendMessageA ::
  -- | Socket
  Fd ->
  -- | Source address (payload A)
  Addr ->
  -- | Length in bytes (payload A)
  CSize ->
  -- | Source and offset (payload B)
  MutableByteArrayOffset RealWorld ->
  -- | Length in bytes (payload B)
  CSize ->
  -- | Flags
  MessageFlags 'Send ->
  -- | Number of bytes pushed to send buffer
  IO (Either Errno CSize)
uninterruptibleSendMessageA
  fd
  (Addr addr)
  lenA
  (MutableByteArrayOffset {array, offset})
  lenB
  flags =
    c_unsafe_sendmsg_a fd addr lenA (unMba array) offset lenB flags
      >>= errorsFromSize

{- | Send two payloads (one from managed memory and one from
unmanaged memory) over a network socket.
-}
uninterruptibleSendMessageB ::
  -- | Socket
  Fd ->
  -- | Source and offset (payload B)
  MutableByteArrayOffset RealWorld ->
  -- | Length in bytes (payload B)
  CSize ->
  -- | Source address (payload A)
  Addr ->
  -- | Length in bytes (payload A)
  CSize ->
  -- | Flags
  MessageFlags 'Send ->
  -- | Number of bytes pushed to send buffer
  IO (Either Errno CSize)
uninterruptibleSendMessageB
  fd
  (MutableByteArrayOffset {array, offset})
  lenB
  (Addr addr)
  lenA
  flags =
    c_unsafe_sendmsg_b fd (unMba array) offset lenB addr lenA flags
      >>= errorsFromSize

{- | Send data from a mutable byte array over a network socket. Users
  may specify an offset and a length to send fewer bytes than are
  actually present in the array. Since this uses the safe
  FFI, it allocates a pinned copy of the bytearry if it was not
  already pinned.
-}
sendMutableByteArray ::
  -- | Socket
  Fd ->
  -- | Source byte array
  MutableByteArray RealWorld ->
  -- | Offset into source array
  Int ->
  -- | Length in bytes
  CSize ->
  -- | Flags
  MessageFlags 'Send ->
  -- | Number of bytes pushed to send buffer
  IO (Either Errno CSize)
sendMutableByteArray fd b@(MutableByteArray b#) off len flags =
  if isMutableByteArrayPinned b
    then errorsFromSize =<< c_safe_mutablebytearray_send fd b# off len flags
    else do
      x@(MutableByteArray x#) <- PM.newPinnedByteArray (csizeToInt len)
      PM.copyMutableByteArray x off b 0 (csizeToInt len)
      errorsFromSize =<< c_safe_mutablebytearray_no_offset_send fd x# len flags

{- | Send data from an address over a network socket. This is not guaranteed
  to send the entire length. This uses the safe FFI since
  it may block indefinitely.
-}
send ::
  -- | Connected socket
  Fd ->
  -- | Source address
  Addr ->
  -- | Length in bytes
  CSize ->
  -- | Flags
  MessageFlags 'Send ->
  -- | Number of bytes pushed to send buffer
  IO (Either Errno CSize)
send fd (Addr addr) len flags =
  c_safe_addr_send fd addr len flags >>= errorsFromSize

{- | Send data from an address over a network socket. This uses the unsafe FFI.
  Users of this function should be sure to set flags that prohibit this
  from blocking. On Linux this is accomplished with @O_NONBLOCK@. It is
  often desirable to call 'threadWaitWrite' on a nonblocking socket before
  calling @unsafeSend@ on it.
-}
uninterruptibleSend ::
  -- | Socket
  Fd ->
  -- | Source address
  Addr ->
  -- | Length in bytes
  CSize ->
  -- | Flags
  MessageFlags 'Send ->
  -- | Number of bytes pushed to send buffer
  IO (Either Errno CSize)
uninterruptibleSend fd (Addr addr) len flags =
  c_unsafe_addr_send fd addr len flags >>= errorsFromSize

{- | Send data from a byte array over a network socket. This uses the unsafe FFI;
  considerations pertaining to 'sendUnsafe' apply to this function as well. Users
  may specify a length to send fewer bytes than are actually present in the
  array.
-}
uninterruptibleSendByteArray ::
  -- | Socket
  Fd ->
  -- | Source byte array
  ByteArray ->
  -- | Offset into source array
  Int ->
  -- | Length in bytes
  CSize ->
  -- | Flags
  MessageFlags 'Send ->
  -- | Number of bytes pushed to send buffer
  IO (Either Errno CSize)
uninterruptibleSendByteArray fd (ByteArray b) off len flags =
  c_unsafe_bytearray_send fd b off len flags >>= errorsFromSize

{- | Send data from a mutable byte array over a network socket. This uses the unsafe FFI;
  considerations pertaining to 'sendUnsafe' apply to this function as well. Users
  specify an offset and a length to send fewer bytes than are actually present in the
  array.
-}
uninterruptibleSendMutableByteArray ::
  -- | Socket
  Fd ->
  -- | Source mutable byte array
  MutableByteArray RealWorld ->
  -- | Offset into source array
  Int ->
  -- | Length in bytes
  CSize ->
  -- | Flags
  MessageFlags 'Send ->
  -- | Number of bytes pushed to send buffer
  IO (Either Errno CSize)
uninterruptibleSendMutableByteArray fd (MutableByteArray b) off len flags =
  c_unsafe_mutable_bytearray_send fd b off len flags >>= errorsFromSize

{- | Send data from a byte array over an unconnected network socket.
  This uses the unsafe FFI; considerations pertaining to 'sendToUnsafe'
  apply to this function as well. The offset and length arguments
  cause a slice of the byte array to be sent rather than the entire
  byte array.
-}
uninterruptibleSendToByteArray ::
  -- | Socket
  Fd ->
  -- | Source byte array
  ByteArray ->
  -- | Offset into source array
  Int ->
  -- | Length in bytes
  CSize ->
  -- | Flags
  MessageFlags 'Send ->
  -- | Socket Address
  SocketAddress ->
  -- | Number of bytes pushed to send buffer
  IO (Either Errno CSize)
uninterruptibleSendToByteArray fd (ByteArray b) off len flags (SocketAddress a@(ByteArray a#)) =
  c_unsafe_bytearray_sendto fd b off len flags a# (intToCInt (PM.sizeofByteArray a)) >>= errorsFromSize

{- | Variant of 'uninterruptibleSendToByteArray' that requires
  that @sockaddr_in@ be used as the socket address. This is used to
  avoid allocating a buffer for the socket address when the caller
  knows in advance that they are sending to an IPv4 address.
-}
uninterruptibleSendToInternetByteArray ::
  -- | Socket
  Fd ->
  -- | Source byte array
  ByteArray ->
  -- | Offset into source array
  Int ->
  -- | Length in bytes
  CSize ->
  -- | Flags
  MessageFlags 'Send ->
  -- | Socket Address
  SocketAddressInternet ->
  -- | Number of bytes pushed to send buffer
  IO (Either Errno CSize)
uninterruptibleSendToInternetByteArray fd (ByteArray b) off len flags (SocketAddressInternet {port, address}) =
  c_unsafe_bytearray_sendto_inet fd b off len flags port address >>= errorsFromSize

{- | Variant of 'uninterruptibleSendToByteArray' that requires
  that @sockaddr_in@ be used as the socket address. This is used to
  avoid allocating a buffer for the socket address when the caller
  knows in advance that they are sending to an IPv4 address.
-}
uninterruptibleSendToInternet ::
  -- | Socket
  Fd ->
  -- | Source byte array
  Addr ->
  -- | Length in bytes
  CSize ->
  -- | Flags
  MessageFlags 'Send ->
  -- | Socket Address
  SocketAddressInternet ->
  -- | Number of bytes pushed to send buffer
  IO (Either Errno CSize)
uninterruptibleSendToInternet fd (Addr b) len flags (SocketAddressInternet {port, address}) =
  c_unsafe_addr_sendto_inet fd b len flags port address >>= errorsFromSize

{- | Send data from a mutable byte array over an unconnected network socket.
  This uses the unsafe FFI; concerns pertaining to 'uninterruptibleSend'
  apply to this function as well. The offset and length arguments
  cause a slice of the mutable byte array to be sent rather than the entire
  byte array.
-}
uninterruptibleSendToMutableByteArray ::
  -- | Socket
  Fd ->
  -- | Source byte array
  MutableByteArray RealWorld ->
  -- | Offset into source array
  Int ->
  -- | Length in bytes
  CSize ->
  -- | Flags
  MessageFlags 'Send ->
  -- | Socket Address
  SocketAddress ->
  -- | Number of bytes pushed to send buffer
  IO (Either Errno CSize)
uninterruptibleSendToMutableByteArray fd (MutableByteArray b) off len flags (SocketAddress a@(ByteArray a#)) =
  c_unsafe_mutable_bytearray_sendto fd b off len flags a# (intToCInt (PM.sizeofByteArray a)) >>= errorsFromSize

{- | Variant of 'uninterruptibleSendToMutableByteArray' that requires
  that @sockaddr_in@ be used as the socket address. This is used to
  avoid allocating a buffer for the socket address when the caller
  knows in advance that they are sending to an IPv4 address.
-}
uninterruptibleSendToInternetMutableByteArray ::
  -- | Socket
  Fd ->
  -- | Source byte array
  MutableByteArray RealWorld ->
  -- | Offset into source array
  Int ->
  -- | Length in bytes
  CSize ->
  -- | Flags
  MessageFlags 'Send ->
  -- | Socket Address
  SocketAddressInternet ->
  -- | Number of bytes pushed to send buffer
  IO (Either Errno CSize)
uninterruptibleSendToInternetMutableByteArray fd (MutableByteArray b) off len flags (SocketAddressInternet {port, address}) =
  c_unsafe_mutable_bytearray_sendto_inet fd b off len flags port address >>= errorsFromSize

{- | Receive data into an address from a network socket. This wraps @recv@ using
  the safe FFI. When the returned size is zero, there are no
  additional bytes to receive and the peer has performed an orderly shutdown.
-}
receive ::
  -- | Socket
  Fd ->
  -- | Source address
  Addr ->
  -- | Length in bytes
  CSize ->
  -- | Flags
  MessageFlags 'Receive ->
  IO (Either Errno CSize)
receive fd (Addr addr) len flags =
  c_safe_addr_recv fd addr len flags >>= errorsFromSize

{- | Receive data into a byte array from a network socket. This wraps @recv@ using
  the safe FFI. When the returned size is zero, there are no
  additional bytes to receive and the peer has performed an orderly shutdown.
-}
receiveByteArray ::
  -- | Socket
  Fd ->
  -- | Length in bytes
  CSize ->
  -- | Flags
  MessageFlags 'Receive ->
  IO (Either Errno ByteArray)
receiveByteArray !fd !len !flags = do
  m <- PM.newPinnedByteArray (csizeToInt len)
  let !(Addr addr) = ptrToAddr (PM.mutableByteArrayContents m)
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

{- | Receive data into an address from a network socket. This wraps @recv@
  using the unsafe FFI. Users of this function should be sure to set flags
  that prohibit this from blocking. On Linux this is accomplished by setting
  the @MSG_DONTWAIT@ flag and handling the resulting @EAGAIN@ or
  @EWOULDBLOCK@. When the returned size is zero, there are no additional
  bytes to receive and the peer has performed an orderly shutdown.
-}
uninterruptibleReceive ::
  -- | Socket
  Fd ->
  -- | Source address
  Addr ->
  -- | Length in bytes
  CSize ->
  -- | Flags
  MessageFlags 'Receive ->
  IO (Either Errno CSize)
{-# INLINE uninterruptibleReceive #-}
uninterruptibleReceive !fd (Addr !addr) !len !flags =
  c_unsafe_addr_recv fd addr len flags >>= errorsFromSize

{- | Receive data into an address from a network socket. This uses the unsafe
  FFI; considerations pertaining to 'receiveUnsafe' apply to this function
  as well. Users may specify a length to receive fewer bytes than are
  actually present in the mutable byte array.
-}
uninterruptibleReceiveMutableByteArray ::
  -- | Socket
  Fd ->
  -- | Destination byte array
  MutableByteArray RealWorld ->
  -- | Destination offset
  Int ->
  -- | Maximum bytes to receive
  CSize ->
  -- | Flags
  MessageFlags 'Receive ->
  -- | Bytes received into array
  IO (Either Errno CSize)
{-# INLINE uninterruptibleReceiveMutableByteArray #-}
uninterruptibleReceiveMutableByteArray !fd (MutableByteArray !b) !off !len !flags =
  c_unsafe_mutable_byte_array_recv fd b off len flags >>= errorsFromSize

{- | Receive data into an address from an unconnected network socket. This
  uses the unsafe FFI. Users may specify an offset into the destination
  byte array. This function does not resize the buffer.
-}
uninterruptibleReceiveFromMutableByteArray ::
  -- | Socket
  Fd ->
  -- | Destination byte array
  MutableByteArray RealWorld ->
  -- | Destination offset
  Int ->
  -- | Maximum bytes to receive
  CSize ->
  -- | Flags
  MessageFlags 'Receive ->
  -- | Maximum socket address size
  CInt ->
  -- | Remote host, bytes received into array, bytes needed for @addrlen@.
  IO (Either Errno (CInt, SocketAddress, CSize))
{-# INLINE uninterruptibleReceiveFromMutableByteArray #-}
-- GHC does not inline this unless we give it the pragma. We really
-- want this to inline since inlining typically avoids the Left/Right
-- data constructor allocation.
uninterruptibleReceiveFromMutableByteArray !fd (MutableByteArray !b) !off !len !flags !maxSz = do
  -- TODO: We currently allocate one buffer for the size and
  -- one for the peer. We could improve this by allocating
  -- a single buffer instead. We would need to add some
  -- cleverness in the cbits directory.
  sockAddrBuf@(MutableByteArray sockAddrBuf#) <- PM.newByteArray (cintToInt maxSz)
  lenBuf@(MutableByteArray lenBuf#) <- PM.newByteArray (PM.sizeOf (undefined :: CInt))
  PM.writeByteArray lenBuf 0 maxSz
  r <- c_unsafe_mutable_byte_array_recvfrom fd b off len flags sockAddrBuf# lenBuf#
  if r > (-1)
    then do
      (sz :: CInt) <- PM.readByteArray lenBuf 0
      if sz < maxSz
        then shrinkMutableByteArray sockAddrBuf (cintToInt sz)
        else pure ()
      sockAddr <- PM.unsafeFreezeByteArray sockAddrBuf
      pure (Right (sz, SocketAddress sockAddr, cssizeToCSize r))
    else fmap Left getErrno

uninterruptibleReceiveFromInternet ::
  -- | Socket
  Fd ->
  -- | Destination byte array
  Addr ->
  -- | Maximum bytes to receive
  CSize ->
  -- | Flags
  MessageFlags 'Receive ->
  -- | Address
  MutablePrimArrayOffset RealWorld SocketAddressInternet ->
  -- | Number of bytes received into array
  IO (Either Errno CSize)
{-# INLINE uninterruptibleReceiveFromInternet #-}
uninterruptibleReceiveFromInternet
  !fd
  (Addr b)
  !len
  !flags
  (MutablePrimArrayOffset (MutablePrimArray sockAddrBuf) addrOff) =
    c_unsafe_recvfrom_inet_addr fd b len flags sockAddrBuf addrOff
      >>= errorsFromSize

uninterruptibleReceiveFromInternetMutableByteArray ::
  -- | Socket
  Fd ->
  -- | Destination byte array
  MutableByteArrayOffset RealWorld ->
  -- | Maximum bytes to receive
  CSize ->
  -- | Flags
  MessageFlags 'Receive ->
  -- | Address
  MutablePrimArrayOffset RealWorld SocketAddressInternet ->
  -- | Number of bytes received into array
  IO (Either Errno CSize)
{-# INLINE uninterruptibleReceiveFromInternetMutableByteArray #-}
uninterruptibleReceiveFromInternetMutableByteArray
  !fd
  (MutableByteArrayOffset (MutableByteArray b) off)
  !len
  !flags
  (MutablePrimArrayOffset (MutablePrimArray sockAddrBuf) addrOff) =
    c_unsafe_recvfrom_inet fd b off len flags sockAddrBuf addrOff
      >>= errorsFromSize

{- | Receive data into an address from a network socket. This uses the unsafe
  FFI. This does not return the socket address of the remote host that
  sent the packet received.
-}
uninterruptibleReceiveFromMutableByteArray_ ::
  -- | Socket
  Fd ->
  -- | Destination byte array
  MutableByteArray RealWorld ->
  -- | Destination offset
  Int ->
  -- | Maximum bytes to receive
  CSize ->
  -- | Flags
  MessageFlags 'Receive ->
  -- | Number of bytes received into array
  IO (Either Errno CSize)
{-# INLINE uninterruptibleReceiveFromMutableByteArray_ #-}
uninterruptibleReceiveFromMutableByteArray_ !fd (MutableByteArray !b) !off !len !flags =
  c_unsafe_mutable_byte_array_peerless_recvfrom fd b off len flags
    >>= errorsFromSize

{- | Receive data into an address from a network socket. This uses the unsafe
  FFI. This does not return the socket address of the remote host that
  sent the packet received.
-}
uninterruptibleReceiveFrom_ ::
  -- | Socket
  Fd ->
  -- | Destination byte array
  Addr ->
  -- | Maximum bytes to receive
  CSize ->
  -- | Flags
  MessageFlags 'Receive ->
  -- | Number of bytes received into array
  IO (Either Errno CSize)
{-# INLINE uninterruptibleReceiveFrom_ #-}
uninterruptibleReceiveFrom_ !fd (Addr !b) !len !flags =
  c_unsafe_addr_peerless_recvfrom fd b len flags
    >>= errorsFromSize

ptrToAddr :: Ptr Word8 -> Addr
ptrToAddr (Exts.Ptr a) = Addr a

-- | Shutdown a socket. This uses the unsafe FFI.
uninterruptibleShutdown ::
  Fd ->
  ShutdownType ->
  IO (Either Errno ())
uninterruptibleShutdown fd typ =
  c_unsafe_shutdown fd typ >>= errorsFromInt_

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

-- Sometimes, functions that return an int use zero to indicate
-- success and negative one to indicate failure without including
-- additional information in the value.
errorsFromInt_ :: CInt -> IO (Either Errno ())
errorsFromInt_ r =
  if r == 0
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

shrinkMutableByteArray :: MutableByteArray RealWorld -> Int -> IO ()
shrinkMutableByteArray (MutableByteArray arr) (I# sz) =
  PM.primitive_ (shrinkMutableByteArray# arr sz)

-- | Convert a 16-bit word from host to network byte order (e.g. @htons@).
hostToNetworkShort :: Word16 -> Word16
hostToNetworkShort = case targetByteOrder of
  BigEndian -> id
  LittleEndian -> byteSwap16

-- | Convert a 16-bit word from network to host byte order (e.g. @ntohs@).
networkToHostShort :: Word16 -> Word16
networkToHostShort = case targetByteOrder of
  BigEndian -> id
  LittleEndian -> byteSwap16

-- | Convert a 32-bit word from host to network byte order (e.g. @htonl@).
hostToNetworkLong :: Word32 -> Word32
hostToNetworkLong = case targetByteOrder of
  BigEndian -> id
  LittleEndian -> byteSwap32

-- | Convert a 32-bit word from network to host byte order (e.g. @ntohl@).
networkToHostLong :: Word32 -> Word32
networkToHostLong = case targetByteOrder of
  BigEndian -> id
  LittleEndian -> byteSwap32

{- $conversion
These functions are used to convert IPv4 addresses and ports between network
byte order and host byte order. They are essential when working with
'SocketAddressInternet'. To avoid getting in the way of GHC compile-time
optimizations, these functions are not actually implemented with FFI
calls to @htonl@ and friends. Rather, they are reimplemented in haskell.
-}

{- $receiveMessage
The function @recvMsg@ presents us with a challenge. Since it uses a
data structure with many nested pointers, we have to use pinned byte
arrays for everything. There is also the difficulty of marshalling
haskell's unlifted array (array of arrays) type into what C's
array of @iovec@. There's the question of the array of @cmsghdr@s.
On top of all of this, we have to answer the question of whether
we want to accept mutable buffer or whether we want to do the
allocations internally (both for the buffers and for the ancilliary
data structurs needed to massage the data into what C expects).

What we do to handle this in offer several variants of @recvmsg@
ending in @A@, @B@, etc.
-}

isByteArrayPinned :: ByteArray -> Bool
{-# INLINE isByteArrayPinned #-}
isByteArrayPinned (ByteArray arr#) =
  Exts.isTrue# (Exts.isByteArrayPinned# arr#)

isMutableByteArrayPinned :: MutableByteArray s -> Bool
{-# INLINE isMutableByteArrayPinned #-}
isMutableByteArrayPinned (MutableByteArray marr#) =
  Exts.isTrue# (Exts.isMutableByteArrayPinned# marr#)

unMba :: MutableByteArray s -> MutableByteArray# s
{-# INLINE unMba #-}
unMba (MutableByteArray x) = x
