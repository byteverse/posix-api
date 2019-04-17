{-# language BangPatterns #-}
{-# language DataKinds #-}
{-# language GADTSyntax #-}
{-# language KindSignatures #-}
{-# language LambdaCase #-}
{-# language MagicHash #-}
{-# language ScopedTypeVariables #-}
{-# language UnboxedTuples #-}
{-# language UnliftedFFITypes #-}

-- | Types and functions related to the POSIX sockets API.
--   Unusual characteristics:
--
--   * Any time the standard calls for @socklen_t@, we use
--     @CInt@ instead. Linus Torvalds <https://yarchive.net/comp/linux/socklen_t.html writes>
--     that \"Any sane library must have @socklen_t@ be the same size as @int@.
--     Anything else breaks any BSD socket layer stuff.\"
--   * Send and receive each have several variants. They are distinguished by
--     the safe\/unsafe FFI use and by the @Addr@\/@ByteArray@/@MutableByteArray@
--     buffer type. They all call @send@ or @recv@ exactly once. They do not
--     repeatedly make syscalls like some of the functions in @network@.
--     Users who want that behavior need to build on top of this package.
--   * There are no requirements on the pinnedness of @ByteArray@ arguments
--     passed to any of these functions. If wrappers of the safe FFI are
--     passed unpinned @ByteArray@ arguments, they will copy the contents
--     into pinned memory before invoking the foreign function.
module Posix.Socket
  ( -- * Functions
    -- ** Socket
    uninterruptibleSocket
    -- ** Socket Pair
  , uninterruptibleSocketPair
    -- ** Bind
  , uninterruptibleBind
    -- ** Connect
  , connect
  , uninterruptibleConnect
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
  , uninterruptibleSetSocketOptionInt
    -- ** Close
  , close
  , uninterruptibleClose
  , uninterruptibleErrorlessClose
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
    -- ** Write Vector
  , writeVector
    -- ** Receive
  , receive
  , receiveByteArray
  , uninterruptibleReceive
  , uninterruptibleReceiveMutableByteArray
    -- ** Receive From
  , uninterruptibleReceiveFromMutableByteArray
  , uninterruptibleReceiveFromMutableByteArray_
    -- ** Receive Message
    -- $receiveMessage
  , uninterruptibleReceiveMessageA
  , uninterruptibleReceiveMessageB
    -- ** Byte-Order Conversion
    -- $conversion
  , hostToNetworkLong
  , hostToNetworkShort
  , networkToHostLong
  , networkToHostShort
    -- * Types
  , Domain(..)
  , Type(..)
  , Protocol(..)
  , OptionName(..)
  , OptionValue(..)
  , Level(..)
  , Message(..)
  , MessageFlags(..)
  , ShutdownType(..)
    -- * Socket Address
    -- ** Types
  , SocketAddress(..)
  , PST.SocketAddressInternet(..)
  , PST.SocketAddressUnix(..)
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
  , PST.unix
  , PST.unspecified
  , PST.internet
  , PST.internet6
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
  , PST.broadcast
    -- ** Message Header
    -- *** Peek
  , PST.peekMessageHeaderName
  , PST.peekMessageHeaderNameLength
  , PST.peekMessageHeaderIOVector
  , PST.peekMessageHeaderIOVectorLength
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

import GHC.ByteOrder (ByteOrder(BigEndian,LittleEndian),targetByteOrder)
import GHC.IO (IO(..))
import Data.Primitive (MutablePrimArray(..),MutableByteArray(..),Addr(..),ByteArray(..))
import Data.Primitive (MutableUnliftedArray(..),UnliftedArray(..))
import Data.Word (Word16,Word32,byteSwap16,byteSwap32)
import Data.Void (Void)
import Foreign.C.Error (Errno,getErrno)
import Foreign.C.Types (CInt(..),CSize(..))
import Foreign.Ptr (nullPtr)
import GHC.Exts (Ptr,RealWorld,ByteArray#,MutableByteArray#)
import GHC.Exts (Addr#,TYPE,RuntimeRep(UnliftedRep))
import GHC.Exts (ArrayArray#,MutableArrayArray#,Int(I#))
import GHC.Exts (shrinkMutableByteArray#,touch#)
import Posix.Socket.Types (Domain(..),Protocol(..),Type(..),SocketAddress(..))
import Posix.Socket.Types (MessageFlags(..),Message(..),ShutdownType(..))
import Posix.Socket.Types (Level(..),OptionName(..),OptionValue(..))
import System.Posix.Types (Fd(..),CSsize(..))

import qualified Posix.Socket.Types as PST
import qualified Data.Primitive as PM
import qualified Control.Monad.Primitive as PM
import qualified GHC.Exts as Exts

-- This module include operating-system specific code used
-- to serialize some of various kind of socket address types.
import qualified Posix.Socket.Platform as PSP

foreign import ccall unsafe "sys/socket.h socket"
  c_socket :: Domain -> Type -> Protocol -> IO Fd

foreign import ccall unsafe "sys/socket.h socketpair"
  c_socketpair :: Domain -> Type -> Protocol -> MutableByteArray# RealWorld -> IO CInt

foreign import ccall unsafe "sys/socket.h listen"
  c_listen :: Fd -> CInt -> IO CInt

foreign import ccall safe "unistd.h close"
  c_safe_close :: Fd -> IO CInt

foreign import ccall unsafe "unistd.h close"
  c_unsafe_close :: Fd -> IO CInt

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
  c_safe_accept :: Fd
                -> MutableByteArray# RealWorld -- SocketAddress
                -> MutableByteArray# RealWorld -- Ptr CInt
                -> IO Fd
foreign import ccall unsafe "sys/socket.h accept"
  c_unsafe_accept :: Fd
                  -> MutableByteArray# RealWorld -- SocketAddress
                  -> MutableByteArray# RealWorld -- Ptr CInt
                  -> IO Fd
-- This variant of accept is used when we do not care about the
-- remote sockaddr. We pass null.
foreign import ccall safe "sys/socket.h accept"
  c_safe_ptr_accept :: Fd -> Ptr Void -> Ptr CInt -> IO Fd

foreign import ccall unsafe "sys/socket.h getsockname"
  c_unsafe_getsockname :: Fd
                       -> MutableByteArray# RealWorld -- SocketAddress
                       -> MutableByteArray# RealWorld -- Addr length (Ptr CInt)
                       -> IO CInt

foreign import ccall unsafe "sys/socket.h getsockopt"
  c_unsafe_getsockopt :: Fd
                      -> Level
                      -> OptionName
                      -> MutableByteArray# RealWorld -- Option value
                      -> MutableByteArray# RealWorld -- Option len (Ptr CInt)
                      -> IO CInt

foreign import ccall unsafe "sys/socket.h setsockopt_int"
  c_unsafe_setsockopt_int :: Fd
                          -> Level
                          -> OptionName
                          -> CInt -- option_value
                          -> IO CInt

-- Per the spec the type signature of connect is:
--   int connect(int sockfd, const struct sockaddr *addr, socklen_t addrlen);
-- The bytearray argument is actually SocketAddress.
foreign import ccall safe "sys/socket.h connect"
  c_safe_connect :: Fd -> ByteArray# -> CInt -> IO CInt
foreign import ccall safe "sys/socket.h connect"
  c_safe_mutablebytearray_connect :: Fd -> MutableByteArray# RealWorld -> CInt -> IO CInt
foreign import ccall unsafe "sys/socket.h connect"
  c_unsafe_connect :: Fd -> ByteArray# -> CInt -> IO CInt

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
  c_safe_bytearray_send :: Fd -> ByteArray# -> CInt -> CSize -> MessageFlags 'Send -> IO CSsize
foreign import ccall safe "sys/socket.h send_offset"
  c_safe_mutablebytearray_send :: Fd -> MutableByteArray# RealWorld -> CInt -> CSize -> MessageFlags 'Send -> IO CSsize
foreign import ccall safe "sys/socket.h send"
  c_safe_mutablebytearray_no_offset_send :: Fd -> MutableByteArray# RealWorld -> CSize -> MessageFlags 'Send -> IO CSsize
foreign import ccall unsafe "sys/socket.h send"
  c_unsafe_addr_send :: Fd -> Addr# -> CSize -> MessageFlags 'Send -> IO CSsize
foreign import ccall unsafe "sys/socket.h send_offset"
  c_unsafe_bytearray_send :: Fd -> ByteArray# -> CInt -> CSize -> MessageFlags 'Send -> IO CSsize
foreign import ccall unsafe "sys/socket.h send_offset"
  c_unsafe_mutable_bytearray_send :: Fd -> MutableByteArray# RealWorld -> CInt -> CSize -> MessageFlags 'Send -> IO CSsize

-- The ByteArray# (second to last argument) is a SocketAddress.
foreign import ccall unsafe "sys/socket.h sendto_offset"
  c_unsafe_bytearray_sendto :: Fd -> ByteArray# -> CInt -> CSize -> MessageFlags 'Send -> ByteArray# -> CInt -> IO CSsize
-- The ByteArray# (second to last argument) is a SocketAddress.
foreign import ccall unsafe "sys/socket.h sendto_offset"
  c_unsafe_mutable_bytearray_sendto :: Fd -> MutableByteArray# RealWorld -> CInt -> CSize -> MessageFlags 'Send -> ByteArray# -> CInt -> IO CSsize

foreign import ccall safe "sys/uio.h writev"
  c_safe_writev :: Fd -> MutableByteArray# RealWorld -> CInt -> IO CSsize

-- There are several ways to wrap recv.
foreign import ccall safe "sys/socket.h recv"
  c_safe_addr_recv :: Fd -> Addr# -> CSize -> MessageFlags 'Receive -> IO CSsize
foreign import ccall unsafe "sys/socket.h recv"
  c_unsafe_addr_recv :: Fd -> Addr# -> CSize -> MessageFlags 'Receive -> IO CSsize
foreign import ccall unsafe "sys/socket.h recv_offset"
  c_unsafe_mutable_byte_array_recv :: Fd -> MutableByteArray# RealWorld -> CInt -> CSize -> MessageFlags 'Receive -> IO CSsize

-- The last two arguments are SocketAddress and Ptr CInt.
foreign import ccall unsafe "sys/socket.h recvfrom_offset"
  c_unsafe_mutable_byte_array_recvfrom :: Fd -> MutableByteArray# RealWorld -> CInt -> CSize -> MessageFlags 'Receive -> MutableByteArray# RealWorld -> MutableByteArray# RealWorld -> IO CSsize
foreign import ccall unsafe "sys/socket.h recvfrom_offset"
  c_unsafe_mutable_byte_array_ptr_recvfrom :: Fd -> MutableByteArray# RealWorld -> CInt -> CSize -> MessageFlags 'Receive -> Ptr Void -> Ptr CInt -> IO CSsize

foreign import ccall unsafe "sys/socket.h recvmsg"
  c_unsafe_addr_recvmsg :: Fd
                        -> Addr# -- This addr is a pointer to msghdr
                        -> MessageFlags 'Receive
                        -> IO CSsize

-- | Create an endpoint for communication, returning a file
--   descriptor that refers to that endpoint. The
--   <http://pubs.opengroup.org/onlinepubs/9699919799/functions/socket.html POSIX specification>
--   includes more details. No special preparation is required before calling
--   this function. The author believes that it cannot block for a prolonged
--   period of time.
uninterruptibleSocket ::
     Domain -- ^ Communications domain (e.g. 'internet', 'unix')
  -> Type -- ^ Socket type (e.g. 'datagram', 'stream') with flags
  -> Protocol -- ^ Protocol
  -> IO (Either Errno Fd)
uninterruptibleSocket dom typ prot = c_socket dom typ prot >>= errorsFromFd

-- | Create an unbound pair of connected sockets in a specified domain, of
--   a specified type, under the protocol optionally specified by the protocol
--   argument. The <http://pubs.opengroup.org/onlinepubs/9699919799/functions/socketpair.html POSIX specification>
--   includes more details. No special preparation is required before calling
--   this function. The author believes that it cannot block for a prolonged
--   period of time.
uninterruptibleSocketPair ::
     Domain -- ^ Communications domain (probably 'unix')
  -> Type -- ^ Socket type (e.g. 'datagram', 'stream') with flags
  -> Protocol -- ^ Protocol
  -> IO (Either Errno (Fd,Fd))
uninterruptibleSocketPair dom typ prot = do
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
--   be used with @sockaddr@ extensions on various platforms. No special
--   preparation is required before calling this function. The author
--   believes that it cannot block for a prolonged period of time.
uninterruptibleBind ::
     Fd -- ^ Socket
  -> SocketAddress -- ^ Socket address, extensible tagged union
  -> IO (Either Errno ())
uninterruptibleBind fd (SocketAddress b@(ByteArray b#)) =
  c_bind fd b# (intToCInt (PM.sizeofByteArray b)) >>= errorsFromInt_

-- | Mark the socket as a passive socket, that is, as a socket that
--   will be used to accept incoming connection requests using @accept@.
--   The <http://pubs.opengroup.org/onlinepubs/9699919799/functions/listen.html POSIX specification>
--   includes more details. No special preparation is required before
--   calling this function. The author believes that it cannot block
--   for a prolonged period of time.
uninterruptibleListen ::
     Fd -- ^ Socket
  -> CInt -- ^ Backlog
  -> IO (Either Errno ())
uninterruptibleListen fd backlog = c_listen fd backlog >>= errorsFromInt_

-- | Connect the socket to the specified socket address.
--   The <http://pubs.opengroup.org/onlinepubs/9699919799/functions/connect.html POSIX specification>
--   includes more details.
connect ::
     Fd -- ^ Fd
  -> SocketAddress -- ^ Socket address, extensible tagged union
  -> IO (Either Errno ())
connect fd (SocketAddress sockAddr@(ByteArray sockAddr#)) =
  case isByteArrayPinned sockAddr of
    True -> c_safe_connect fd sockAddr# (intToCInt (PM.sizeofByteArray sockAddr)) >>= errorsFromInt_
    False -> do
      let len = PM.sizeofByteArray sockAddr
      x@(MutableByteArray x#) <- PM.newPinnedByteArray len
      PM.copyByteArray x 0 sockAddr 0 len
      c_safe_mutablebytearray_connect fd x# (intToCInt len) >>= errorsFromInt_

-- | Connect the socket to the specified socket address.
--   The <http://pubs.opengroup.org/onlinepubs/9699919799/functions/connect.html POSIX specification>
--   includes more details. The only sensible way to use this is to
--   give a nonblocking socket as the argument.
uninterruptibleConnect ::
     Fd -- ^ Fd
  -> SocketAddress -- ^ Socket address, extensible tagged union
  -> IO (Either Errno ())
uninterruptibleConnect fd (SocketAddress sockAddr@(ByteArray sockAddr#)) =
  c_unsafe_connect fd sockAddr# (intToCInt (PM.sizeofByteArray sockAddr)) >>= errorsFromInt_

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
  -> IO (Either Errno (CInt,SocketAddress,Fd)) -- ^ Peer information and connected socket
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
      -- sockAddr <- PM.unsafeFreezeByteArray =<< PM.resizeMutableByteArray sockAddrBuf (cintToInt sz)
      pure (Right (sz,SocketAddress sockAddr,r))
    else fmap Left getErrno

-- | See 'accept'. This uses the unsafe FFI. Consequently, it does not
--   not need to allocate pinned memory. It only makes sense to call this
--   on a nonblocking socket.
uninterruptibleAccept ::
     Fd -- ^ Listening socket
  -> CInt -- ^ Maximum socket address size
  -> IO (Either Errno (CInt,SocketAddress,Fd)) -- ^ Peer information and connected socket
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
      pure (Right (sz,SocketAddress sockAddr,r))
    else fmap Left getErrno

-- | A variant of 'accept' that does not provide the user with a
--   'SocketAddress' detailing the peer.
accept_ ::
     Fd -- ^ Listening socket
  -> IO (Either Errno Fd) -- ^ Connected socket
accept_ sock =
  c_safe_ptr_accept sock nullPtr nullPtr >>= errorsFromFd

-- | Retrieve the locally-bound name of the specified socket. The
--   <http://pubs.opengroup.org/onlinepubs/9699919799/functions/accept.html POSIX specification>
--   of @getsockname@ includes more details.
uninterruptibleGetSocketName ::
     Fd -- ^ Socket
  -> CInt -- ^ Maximum socket address size
  -> IO (Either Errno (CInt,SocketAddress))
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
      pure (Right (sz,SocketAddress sockAddr))
    else fmap Left getErrno

-- | Retrieve the value for the option specified by the 'Option' argument for
--   the socket specified by the 'Fd' argument. The
--   <http://pubs.opengroup.org/onlinepubs/9699919799/functions/getsockopt.html POSIX specification>
--   of @getsockopt@ includes more details.
uninterruptibleGetSocketOption ::
     Fd -- ^ Socket
  -> Level -- ^ Socket level
  -> OptionName -- Option name
  -> CInt -- ^ Maximum option value size
  -> IO (Either Errno (CInt,OptionValue))
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
      pure (Right (sz,OptionValue value))
    else fmap Left getErrno

-- | Set the value for the option specified by the 'Option' argument for
--   the socket specified by the 'Fd' argument. The
--   <http://pubs.opengroup.org/onlinepubs/9699919799/functions/getsockopt.html POSIX specification>
--   of @getsockopt@ includes more details. This variant requires that the
--   size of the @option_value@
--   be the same as the size of 'CInt'. That is, the @option_name@ must
--   describe an option that is represented by a C integer. This is a
--   common case, so we avoid allocations by reference-passing in C.
uninterruptibleSetSocketOptionInt ::
     Fd -- ^ Socket
  -> Level -- ^ Socket level
  -> OptionName -- ^ Option name
  -> CInt -- ^ Option value
  -> IO (Either Errno ())
uninterruptibleSetSocketOptionInt sock level optName optValue =
  c_unsafe_setsockopt_int sock level optName optValue >>= errorsFromInt_

-- | Send data from a byte array over a network socket. Users
--   may specify an offset and a length to send fewer bytes than are
--   actually present in the array. Since this uses the safe
--   FFI, it allocates a pinned copy of the bytearry if it was not
--   already pinned.
sendByteArray ::
     Fd -- ^ Socket
  -> ByteArray -- ^ Source byte array
  -> CInt -- ^ Offset into source array
  -> CSize -- ^ Length in bytes
  -> MessageFlags 'Send -- ^ Flags
  -> IO (Either Errno CSize) -- ^ Number of bytes pushed to send buffer
sendByteArray fd b@(ByteArray b#) off len flags = if isByteArrayPinned b
  then errorsFromSize =<< c_safe_bytearray_send fd b# off len flags
  else do
    x@(MutableByteArray x#) <- PM.newPinnedByteArray (csizeToInt len)
    PM.copyByteArray x (cintToInt off) b 0 (csizeToInt len)
    errorsFromSize =<< c_safe_mutablebytearray_no_offset_send fd x# len flags

-- | Write data from multiple byte arrays to the file/socket associated
--   with the file descriptor. This does not support slicing. The
--   <http://pubs.opengroup.org/onlinepubs/009604499/functions/writev.html POSIX specification>
--   of @writev@ includes more details.
writeVector ::
     Fd -- ^ Socket
  -> UnliftedArray ByteArray -- ^ Source byte arrays
  -> IO (Either Errno CSize)
writeVector fd buffers = do
  iovecs@(MutableByteArray iovecs#) :: MutableByteArray RealWorld <-
    PM.newPinnedByteArray
      (cintToInt PST.sizeofIOVector * PM.sizeofUnliftedArray buffers)

  -- We construct a list of the new buffers for the sole purpose
  -- of ensuring that we can touch the list later to keep all
  -- the new buffers live.
  newBufs <- foldDownward (PM.sizeofUnliftedArray buffers) UNil $ \newBufs i -> do
    let !buf = PM.indexUnliftedArray buffers i
    pinByteArray buf >>= \case
      Nothing -> do
        let buffer = buf
        let targetAddr :: Addr
            targetAddr = PM.mutableByteArrayContents iovecs `PM.plusAddr`
              (i * cintToInt PST.sizeofIOVector)
        PST.pokeIOVectorBase targetAddr (PM.byteArrayContents buffer)
        PST.pokeIOVectorLength targetAddr (intToCSize (PM.sizeofByteArray buffer))
        pure newBufs
      Just buffer -> do
        let targetAddr :: Addr
            targetAddr = PM.mutableByteArrayContents iovecs `PM.plusAddr`
              (i * cintToInt PST.sizeofIOVector)
        PST.pokeIOVectorBase targetAddr (PM.byteArrayContents buffer)
        PST.pokeIOVectorLength targetAddr (intToCSize (PM.sizeofByteArray buffer))
        pure (UCons (unByteArray buffer) newBufs)
  r <- errorsFromSize =<<
    c_safe_writev fd iovecs# (intToCInt (PM.sizeofUnliftedArray buffers))
  -- Touching both the unlifted array and the list of new buffers
  -- here is crucial to ensuring that
  -- the buffers do not get GCed before c_safe_writev. Just touching
  -- them should keep all of their children alive too.
  touchUnliftedArray buffers
  touchLifted newBufs
  pure r

data UList (a :: TYPE 'UnliftedRep) where
  UNil :: UList a
  UCons :: a -> UList a -> UList a

-- Internal function. Fold with strict accumulator. Upper bound is exclusive.
-- Hits every int in the range [0,hi) from highest to lowest.
foldDownward :: forall a. Int -> a -> (a -> Int -> IO a) -> IO a
{-# INLINE foldDownward #-}
foldDownward !hi !a0 f = go (hi - 1) a0 where
  go :: Int -> a -> IO a
  go !ix !a = if ix >= 0
    then f a ix >>= go (ix - 1)
    else pure a

-- | Copy and pin a byte array if, it's not already pinned.
pinByteArray :: ByteArray -> IO (Maybe ByteArray)
{-# INLINE pinByteArray #-}
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

-- | Send data from a mutable byte array over a network socket. Users
--   may specify an offset and a length to send fewer bytes than are
--   actually present in the array. Since this uses the safe
--   FFI, it allocates a pinned copy of the bytearry if it was not
--   already pinned.
sendMutableByteArray ::
     Fd -- ^ Socket
  -> MutableByteArray RealWorld -- ^ Source byte array
  -> CInt -- ^ Offset into source array
  -> CSize -- ^ Length in bytes
  -> MessageFlags 'Send -- ^ Flags
  -> IO (Either Errno CSize) -- ^ Number of bytes pushed to send buffer
sendMutableByteArray fd b@(MutableByteArray b#) off len flags = if isMutableByteArrayPinned b
  then errorsFromSize =<< c_safe_mutablebytearray_send fd b# off len flags
  else do
    x@(MutableByteArray x#) <- PM.newPinnedByteArray (csizeToInt len)
    PM.copyMutableByteArray x (cintToInt off) b 0 (csizeToInt len)
    errorsFromSize =<< c_safe_mutablebytearray_no_offset_send fd x# len flags

-- | Send data from an address over a network socket. This is not guaranteed
--   to send the entire length. This uses the safe FFI since
--   it may block indefinitely.
send ::
     Fd -- ^ Connected socket
  -> Addr -- ^ Source address
  -> CSize -- ^ Length in bytes
  -> MessageFlags 'Send -- ^ Flags
  -> IO (Either Errno CSize) -- ^ Number of bytes pushed to send buffer
send fd (Addr addr) len flags =
  c_safe_addr_send fd addr len flags >>= errorsFromSize

-- | Send data from an address over a network socket. This uses the unsafe FFI.
--   Users of this function should be sure to set flags that prohibit this
--   from blocking. On Linux this is accomplished with @O_NONBLOCK@. It is
--   often desirable to call 'threadWaitWrite' on a nonblocking socket before
--   calling @unsafeSend@ on it.
uninterruptibleSend ::
     Fd -- ^ Socket
  -> Addr -- ^ Source address
  -> CSize -- ^ Length in bytes
  -> MessageFlags 'Send -- ^ Flags
  -> IO (Either Errno CSize) -- ^ Number of bytes pushed to send buffer
uninterruptibleSend fd (Addr addr) len flags =
  c_unsafe_addr_send fd addr len flags >>= errorsFromSize

-- | Send data from a byte array over a network socket. This uses the unsafe FFI;
--   considerations pertaining to 'sendUnsafe' apply to this function as well. Users
--   may specify a length to send fewer bytes than are actually present in the
--   array.
uninterruptibleSendByteArray ::
     Fd -- ^ Socket
  -> ByteArray -- ^ Source byte array
  -> CInt -- ^ Offset into source array
  -> CSize -- ^ Length in bytes
  -> MessageFlags 'Send -- ^ Flags
  -> IO (Either Errno CSize) -- ^ Number of bytes pushed to send buffer
uninterruptibleSendByteArray fd (ByteArray b) off len flags =
  c_unsafe_bytearray_send fd b off len flags >>= errorsFromSize

-- | Send data from a mutable byte array over a network socket. This uses the unsafe FFI;
--   considerations pertaining to 'sendUnsafe' apply to this function as well. Users
--   specify an offset and a length to send fewer bytes than are actually present in the
--   array.
uninterruptibleSendMutableByteArray ::
     Fd -- ^ Socket
  -> MutableByteArray RealWorld -- ^ Source mutable byte array
  -> CInt -- ^ Offset into source array
  -> CSize -- ^ Length in bytes
  -> MessageFlags 'Send -- ^ Flags
  -> IO (Either Errno CSize) -- ^ Number of bytes pushed to send buffer
uninterruptibleSendMutableByteArray fd (MutableByteArray b) off len flags =
  c_unsafe_mutable_bytearray_send fd b off len flags >>= errorsFromSize

-- | Send data from a byte array over an unconnected network socket.
--   This uses the unsafe FFI; considerations pertaining to 'sendToUnsafe'
--   apply to this function as well. The offset and length arguments
--   cause a slice of the byte array to be sent rather than the entire
--   byte array.
uninterruptibleSendToByteArray ::
     Fd -- ^ Socket
  -> ByteArray -- ^ Source byte array
  -> CInt -- ^ Offset into source array
  -> CSize -- ^ Length in bytes
  -> MessageFlags 'Send -- ^ Flags
  -> SocketAddress -- ^ Socket Address
  -> IO (Either Errno CSize) -- ^ Number of bytes pushed to send buffer
uninterruptibleSendToByteArray fd (ByteArray b) off len flags (SocketAddress a@(ByteArray a#)) =
  c_unsafe_bytearray_sendto fd b off len flags a# (intToCInt (PM.sizeofByteArray a)) >>= errorsFromSize

-- | Send data from a mutable byte array over an unconnected network socket.
--   This uses the unsafe FFI; considerations pertaining to 'sendToUnsafe'
--   apply to this function as well. The offset and length arguments
--   cause a slice of the mutable byte array to be sent rather than the entire
--   byte array.
uninterruptibleSendToMutableByteArray ::
     Fd -- ^ Socket
  -> MutableByteArray RealWorld -- ^ Source byte array
  -> CInt -- ^ Offset into source array
  -> CSize -- ^ Length in bytes
  -> MessageFlags 'Send -- ^ Flags
  -> SocketAddress -- ^ Socket Address
  -> IO (Either Errno CSize) -- ^ Number of bytes pushed to send buffer
uninterruptibleSendToMutableByteArray fd (MutableByteArray b) off len flags (SocketAddress a@(ByteArray a#)) =
  c_unsafe_mutable_bytearray_sendto fd b off len flags a# (intToCInt (PM.sizeofByteArray a)) >>= errorsFromSize

-- | Receive data into an address from a network socket. This wraps @recv@ using
--   the safe FFI. When the returned size is zero, there are no
--   additional bytes to receive and the peer has performed an orderly shutdown.
receive ::
     Fd -- ^ Socket
  -> Addr -- ^ Source address
  -> CSize -- ^ Length in bytes
  -> MessageFlags 'Receive -- ^ Flags
  -> IO (Either Errno CSize)
receive fd (Addr addr) len flags =
  c_safe_addr_recv fd addr len flags >>= errorsFromSize

-- | Receive data into a byte array from a network socket. This wraps @recv@ using
--   the safe FFI. When the returned size is zero, there are no
--   additional bytes to receive and the peer has performed an orderly shutdown.
receiveByteArray ::
     Fd -- ^ Socket
  -> CSize -- ^ Length in bytes
  -> MessageFlags 'Receive -- ^ Flags
  -> IO (Either Errno ByteArray)
receiveByteArray !fd !len !flags = do
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
uninterruptibleReceive ::
     Fd -- ^ Socket
  -> Addr -- ^ Source address
  -> CSize -- ^ Length in bytes
  -> MessageFlags 'Receive -- ^ Flags
  -> IO (Either Errno CSize)
uninterruptibleReceive !fd (Addr !addr) !len !flags =
  c_unsafe_addr_recv fd addr len flags >>= errorsFromSize

-- | Receive data into an address from a network socket. This uses the unsafe
--   FFI; considerations pertaining to 'receiveUnsafe' apply to this function
--   as well. Users may specify a length to receive fewer bytes than are
--   actually present in the mutable byte array.
uninterruptibleReceiveMutableByteArray ::
     Fd -- ^ Socket
  -> MutableByteArray RealWorld -- ^ Destination byte array
  -> CInt -- ^ Destination offset
  -> CSize -- ^ Maximum bytes to receive
  -> MessageFlags 'Receive -- ^ Flags
  -> IO (Either Errno CSize) -- ^ Bytes received into array
uninterruptibleReceiveMutableByteArray !fd (MutableByteArray !b) !off !len !flags =
  c_unsafe_mutable_byte_array_recv fd b off len flags >>= errorsFromSize

-- | Receive data into an address from an unconnected network socket. This
--   uses the unsafe FFI. Users may specify an offset into the destination
--   byte array. This function does not resize the buffer.
uninterruptibleReceiveFromMutableByteArray ::
     Fd -- ^ Socket
  -> MutableByteArray RealWorld -- ^ Destination byte array
  -> CInt -- ^ Destination offset
  -> CSize -- ^ Maximum bytes to receive
  -> MessageFlags 'Receive -- ^ Flags
  -> CInt -- ^ Maximum socket address size
  -> IO (Either Errno (CInt,SocketAddress,CSize))
     -- ^ Remote host, bytes received into array, bytes needed for @addrlen@.
{-# INLINE uninterruptibleReceiveFromMutableByteArray #-}
-- GHC does not inline this unless we give it the pragma. We really
-- want this to inline since inlining typically avoids the Left/Right
-- data constructor allocation.
uninterruptibleReceiveFromMutableByteArray !fd (MutableByteArray !b) !off !len !flags !maxSz = do
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
      pure (Right (sz,SocketAddress sockAddr,cssizeToCSize r))
    else fmap Left getErrno

-- | Receive data into an address from a network socket. This uses the unsafe
--   FFI. This does not return the socket address of the remote host that
--   sent the packet received.
uninterruptibleReceiveFromMutableByteArray_ ::
     Fd -- ^ Socket
  -> MutableByteArray RealWorld -- ^ Destination byte array
  -> CInt -- ^ Destination offset
  -> CSize -- ^ Maximum bytes to receive
  -> MessageFlags 'Receive -- ^ Flags
  -> IO (Either Errno CSize) -- ^ Number of bytes received into array
uninterruptibleReceiveFromMutableByteArray_ !fd (MutableByteArray !b) !off !len !flags =
  c_unsafe_mutable_byte_array_ptr_recvfrom fd b off len flags nullPtr nullPtr >>= errorsFromSize



-- | Receive a message, scattering the input. This does not provide
--   the socket address or the control messages. All of the chunks
--   must have the same maximum size. All resulting byte arrays have
--   been explicitly pinned.
uninterruptibleReceiveMessageA ::
     Fd -- ^ Socket
  -> CSize -- ^ Maximum bytes per chunk
  -> CSize -- ^ Maximum number of chunks
  -> MessageFlags 'Receive -- ^ Flags
  -> IO (Either Errno (CSize,UnliftedArray ByteArray))
uninterruptibleReceiveMessageA !s !chunkSize !chunkCount !flags = do
  bufs <- PM.unsafeNewUnliftedArray (csizeToInt chunkCount)
  iovecsBuf <- PM.newPinnedByteArray (csizeToInt chunkCount * cintToInt PST.sizeofIOVector)
  let iovecsAddr = PM.mutableByteArrayContents iovecsBuf
  initializeIOVectors bufs iovecsAddr chunkSize chunkCount
  msgHdrBuf <- PM.newPinnedByteArray (cintToInt PST.sizeofMessageHeader)
  let !msgHdrAddr@(Addr msgHdrAddr#) = PM.mutableByteArrayContents msgHdrBuf
  pokeMessageHeader msgHdrAddr PM.nullAddr 0 iovecsAddr chunkCount PM.nullAddr 0 flags
  r <- c_unsafe_addr_recvmsg s msgHdrAddr# flags
  if r > (-1)
    then do
      filled <- countAndShrinkIOVectors (csizeToInt chunkCount) (cssizeToInt r) (csizeToInt chunkSize) bufs
      frozenBufs <- deepFreezeIOVectors filled bufs
      touchMutableUnliftedArray bufs
      touchMutableByteArray iovecsBuf
      touchMutableByteArray msgHdrBuf
      pure (Right (cssizeToCSize r,frozenBufs))
    else do
      touchMutableUnliftedArray bufs
      touchMutableByteArray iovecsBuf
      touchMutableByteArray msgHdrBuf
      fmap Left getErrno

-- | Receive a message, scattering the input. This provides the socket
--   address but does not include control messages. All of the chunks
--   must have the same maximum size. All resulting byte arrays have
--   been explicitly pinned.
uninterruptibleReceiveMessageB ::
     Fd -- ^ Socket
  -> CSize -- ^ Maximum bytes per chunk
  -> CSize -- ^ Maximum number of chunks
  -> MessageFlags 'Receive -- ^ Flags
  -> CInt -- ^ Maximum socket address size
  -> IO (Either Errno (CInt,SocketAddress,CSize,UnliftedArray ByteArray))
uninterruptibleReceiveMessageB !s !chunkSize !chunkCount !flags !maxSockAddrSz = do
  sockAddrBuf <- PM.newPinnedByteArray (cintToInt maxSockAddrSz)
  bufs <- PM.unsafeNewUnliftedArray (csizeToInt chunkCount)
  iovecsBuf <- PM.newPinnedByteArray (csizeToInt chunkCount * cintToInt PST.sizeofIOVector)
  let iovecsAddr = PM.mutableByteArrayContents iovecsBuf
  initializeIOVectors bufs iovecsAddr chunkSize chunkCount
  msgHdrBuf <- PM.newPinnedByteArray (cintToInt PST.sizeofMessageHeader)
  let !msgHdrAddr@(Addr msgHdrAddr#) = PM.mutableByteArrayContents msgHdrBuf
  pokeMessageHeader msgHdrAddr (PM.mutableByteArrayContents sockAddrBuf) maxSockAddrSz iovecsAddr chunkCount PM.nullAddr 0 flags
  r <- c_unsafe_addr_recvmsg s msgHdrAddr# flags
  if r > (-1)
    then do
      actualSockAddrSz <- PST.peekMessageHeaderNameLength msgHdrAddr
      if actualSockAddrSz < maxSockAddrSz
        then shrinkMutableByteArray sockAddrBuf (cintToInt actualSockAddrSz)
        else pure ()
      sockAddr <- PM.unsafeFreezeByteArray sockAddrBuf
      filled <- countAndShrinkIOVectors (csizeToInt chunkCount) (cssizeToInt r) (csizeToInt chunkSize) bufs
      frozenBufs <- deepFreezeIOVectors filled bufs
      touchMutableUnliftedArray bufs
      touchMutableByteArray iovecsBuf
      touchMutableByteArray msgHdrBuf
      touchMutableByteArray sockAddrBuf
      pure (Right (actualSockAddrSz,SocketAddress sockAddr,cssizeToCSize r,frozenBufs))
    else do
      touchMutableUnliftedArray bufs
      touchMutableByteArray iovecsBuf
      touchMutableByteArray msgHdrBuf
      touchMutableByteArray sockAddrBuf
      fmap Left getErrno

-- | Close a socket. The <http://pubs.opengroup.org/onlinepubs/009696899/functions/close.html POSIX specification>
--   includes more details. This uses the safe FFI.
close ::
     Fd -- ^ Socket
  -> IO (Either Errno ())
close fd = c_safe_close fd >>= errorsFromInt_

-- | Close a socket. This uses the unsafe FFI. According to the
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

-- | Close a socket with the unsafe FFI. Do not check for errors. It is only
--   appropriate to use this when a socket is being closed to handle an
--   exceptional case. Since the user will want the propogate the original
--   exception, the exception provided by 'uninterruptibleClose' would just
--   be discarded. This function allows us to potentially avoid an additional
--   FFI call to 'getErrno'.
uninterruptibleErrorlessClose ::
     Fd -- ^ Socket
  -> IO ()
uninterruptibleErrorlessClose fd = do
  _ <- c_unsafe_close fd
  pure ()

-- | Shutdown a socket. This uses the unsafe FFI.
uninterruptibleShutdown ::
     Fd
  -> ShutdownType
  -> IO (Either Errno ())
uninterruptibleShutdown fd typ =
  c_unsafe_shutdown fd typ >>= errorsFromInt_

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
errorsFromInt_ :: CInt -> IO (Either Errno ())
errorsFromInt_ r = if r == 0
  then pure (Right ())
  else fmap Left getErrno

intToCInt :: Int -> CInt
intToCInt = fromIntegral

intToCSize :: Int -> CSize
intToCSize = fromIntegral

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

pokeMessageHeader :: Addr -> Addr -> CInt -> Addr -> CSize -> Addr -> CSize -> MessageFlags 'Receive -> IO ()
pokeMessageHeader msgHdrAddr a b c d e f g = do
  PST.pokeMessageHeaderName msgHdrAddr a
  PST.pokeMessageHeaderNameLength msgHdrAddr b
  PST.pokeMessageHeaderIOVector msgHdrAddr c
  PST.pokeMessageHeaderIOVectorLength msgHdrAddr d
  PST.pokeMessageHeaderControl msgHdrAddr e
  PST.pokeMessageHeaderControlLength msgHdrAddr f
  PST.pokeMessageHeaderFlags msgHdrAddr g

-- This sets up an array of iovec. The iov_len is assigned to the
-- same length in all of these. The actual buffers are allocated
-- and stuck in an unlifted array. Pointers to these buffers (we can
-- do that because they are pinned) go in the iov_base field.
initializeIOVectors ::
     MutableUnliftedArray RealWorld (MutableByteArray RealWorld) -- buffers
  -> Addr -- array of iovec
  -> CSize -- chunk size
  -> CSize -- chunk count
  -> IO ()
initializeIOVectors bufs iovecsAddr chunkSize chunkCount =
  let go !ix !iovecAddr = if ix < csizeToInt chunkCount
        then do
          initializeIOVector bufs iovecAddr chunkSize ix
          go (ix + 1) (PM.plusAddr iovecAddr (cintToInt PST.sizeofIOVector))
        else pure ()
   in go 0 iovecsAddr

-- Initialize a single iovec. We write the pinned byte array into
-- both the iov_base field and into an unlifted array. There is a
-- copy of this function in Linux.Socket.
initializeIOVector ::
     MutableUnliftedArray RealWorld (MutableByteArray RealWorld)
  -> Addr
  -> CSize
  -> Int
  -> IO ()
initializeIOVector bufs iovecAddr chunkSize ix = do
  buf <- PM.newPinnedByteArray (csizeToInt chunkSize)
  PM.writeUnliftedArray bufs ix buf
  PST.pokeIOVectorBase iovecAddr (PM.mutableByteArrayContents buf)
  PST.pokeIOVectorLength iovecAddr chunkSize

-- This is intended to be called on an array of iovec after recvmsg
-- and before deepFreezeIOVectors. An adaptation of this function exists
-- in Linux.Socket.
countAndShrinkIOVectors ::
     Int -- Total number of supplied iovecs
  -> Int -- Total amount of space used by receive
  -> Int -- Amount of space per buffer (each buffer must have equal size)
  -> MutableUnliftedArray RealWorld (MutableByteArray RealWorld)
  -> IO Int
countAndShrinkIOVectors !n !totalUsedSz !maxBufSz !bufs = go 0 totalUsedSz where
  -- This outer if (checking that the index is in bounds) should
  -- not actually be necessary. I will remove once the test suite
  -- bolsters my confidence.
  go !ix !remainingBytes = if ix < n
    then if remainingBytes >= maxBufSz
      then go
        (ix + 1)
        (remainingBytes - maxBufSz)
      else if remainingBytes == 0
        then pure ix
        else do
          buf <- PM.readUnliftedArray bufs ix
          shrinkMutableByteArray buf remainingBytes
          pure (ix + 1)
    else pure ix

-- Freeze a slice of the mutable byte arrays inside the unlifted
-- array. This copies makes a copy of the slice of the original
-- array. A copy of this function exists in Linux.Socket.
deepFreezeIOVectors ::
     Int -- How many iovecs actually had a non-zero number of bytes
  -> MutableUnliftedArray RealWorld (MutableByteArray RealWorld)
  -> IO (UnliftedArray ByteArray)
deepFreezeIOVectors n m = do
  x <- PM.unsafeNewUnliftedArray n
  let go !ix = if ix < n
        then do
          PM.writeUnliftedArray x ix =<< PM.unsafeFreezeByteArray =<< PM.readUnliftedArray m ix
          go (ix + 1)
        else PM.unsafeFreezeUnliftedArray x
  go 0

unByteArray :: ByteArray -> ByteArray#
unByteArray (ByteArray x) = x

touchMutableUnliftedArray :: MutableUnliftedArray RealWorld a -> IO ()
touchMutableUnliftedArray (MutableUnliftedArray x) = touchMutableUnliftedArray# x

touchUnliftedArray :: UnliftedArray a -> IO ()
touchUnliftedArray (UnliftedArray x) = touchUnliftedArray# x

touchMutableByteArray :: MutableByteArray RealWorld -> IO ()
touchMutableByteArray (MutableByteArray x) = touchMutableByteArray# x

touchMutableUnliftedArray# :: MutableArrayArray# RealWorld -> IO ()
touchMutableUnliftedArray# x = IO $ \s -> case touch# x s of s' -> (# s', () #)

touchUnliftedArray# :: ArrayArray# -> IO ()
touchUnliftedArray# x = IO $ \s -> case touch# x s of s' -> (# s', () #)

touchMutableByteArray# :: MutableByteArray# RealWorld -> IO ()
touchMutableByteArray# x = IO $ \s -> case touch# x s of s' -> (# s', () #)

touchLifted :: a -> IO ()
touchLifted x = IO $ \s -> case touch# x s of s' -> (# s', () #)

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
{-# inline isByteArrayPinned #-}
isByteArrayPinned (ByteArray arr#) =
  Exts.isTrue# (Exts.isByteArrayPinned# arr#)

isMutableByteArrayPinned :: MutableByteArray s -> Bool
{-# inline isMutableByteArrayPinned #-}
isMutableByteArrayPinned (MutableByteArray marr#) =
  Exts.isTrue# (Exts.isMutableByteArrayPinned# marr#)
