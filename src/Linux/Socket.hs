{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnliftedFFITypes #-}

{- FOURMOLU_DISABLE -}
module Linux.Socket
  ( -- * Functions
    uninterruptibleAccept4
  , uninterruptibleAccept4_
#if defined(UNLIFTEDARRAYFUNCTIONS)
  , uninterruptibleReceiveMultipleMessageA
  , uninterruptibleReceiveMultipleMessageB
  , uninterruptibleReceiveMultipleMessageC
  , uninterruptibleReceiveMultipleMessageD
#endif
    -- * Types
  , SocketFlags(..)
    -- * Option Names
  , LST.headerInclude
    -- * Message Flags
  , LST.dontWait
  , LST.truncate
  , LST.controlTruncate
    -- * Socket Flags
  , LST.closeOnExec
  , LST.nonblocking
    -- * Twiddle
  , applySocketFlags
    -- * UDP Header
  , LST.sizeofUdpHeader
  , LST.pokeUdpHeaderSourcePort
  , LST.pokeUdpHeaderDestinationPort
  , LST.pokeUdpHeaderLength
  , LST.pokeUdpHeaderChecksum
    -- * IPv4 Header
  , LST.sizeofIpHeader
  , LST.pokeIpHeaderVersionIhl
  , LST.pokeIpHeaderTypeOfService
  , LST.pokeIpHeaderTotalLength
  , LST.pokeIpHeaderIdentifier
  , LST.pokeIpHeaderFragmentOffset
  , LST.pokeIpHeaderTimeToLive
  , LST.pokeIpHeaderProtocol
  , LST.pokeIpHeaderChecksum
  , LST.pokeIpHeaderSourceAddress
  , LST.pokeIpHeaderDestinationAddress
  ) where
{- FOURMOLU_ENABLE -}

import Prelude hiding (truncate)

import Data.Bits ((.|.))
import Data.Primitive (MutableByteArray (..))
#if defined(UNLIFTEDARRAYFUNCTIONS)
import Data.Primitive.Unlifted.Array (MutableUnliftedArray,UnliftedArray)
import Data.Primitive.Unlifted.Array (MutableUnliftedArray_(MutableUnliftedArray))
import Data.Primitive.Unlifted.Array.Primops (MutableUnliftedArray#(MutableUnliftedArray#))
#endif
import Data.Void (Void)
import Foreign.C.Error (Errno, getErrno)
import Foreign.C.Types (CInt (..))
import Foreign.Ptr (nullPtr)
import GHC.Exts (Int (I#), MutableByteArray#, Ptr (..), RealWorld, shrinkMutableByteArray#)
import Linux.Socket.Types (SocketFlags (..))
import Posix.Socket (SocketAddress (..), Type (..))
import System.Posix.Types (Fd (..))

import qualified Control.Monad.Primitive as PM
import qualified Data.Primitive as PM
#if defined(UNLIFTEDARRAYFUNCTIONS)
import qualified Data.Primitive.Unlifted.Array as PM
#endif
import qualified Linux.Socket.Types as LST

-- 2024-02-05: Commented out unused function.
-- foreign import ccall unsafe "sys/socket.h recvmmsg"
--   c_unsafe_addr_recvmmsg :: Fd
--                          -> Addr# -- This addr is an array of msghdr
--                          -> CUInt -- Length of msghdr array
--                          -> MessageFlags 'Receive
--                          -> Addr# -- Timeout
--                          -> IO CSsize

foreign import ccall unsafe "sys/socket.h accept4"
  c_unsafe_accept4 ::
    Fd ->
    MutableByteArray# RealWorld -> -- SocketAddress
    MutableByteArray# RealWorld -> -- Ptr CInt
    SocketFlags ->
    IO Fd

-- Variant of c_unsafe_ptr_accept4 that uses Ptr instead of MutableByteArray.
-- Currently, we expect that the two pointers are set to NULL.
-- This is only used internally.
foreign import ccall unsafe "sys/socket.h accept4"
  c_unsafe_ptr_accept4 ::
    Fd ->
    Ptr Void -> -- SocketAddress
    Ptr Void -> -- Ptr CInt
    SocketFlags ->
    IO Fd

#if defined(UNLIFTEDARRAYFUNCTIONS)
foreign import ccall unsafe "HaskellPosix.h recvmmsg_sockaddr_in"
  c_unsafe_recvmmsg_sockaddr_in ::
       Fd
    -> MutableByteArray# RealWorld -- lengths
    -> MutableByteArray# RealWorld -- sockaddrs
    -> MutableArray# RealWorld (MutableByteArray# RealWorld) -- buffers
    -> CUInt -- Length of msghdr array
    -> MessageFlags 'Receive
    -> IO CInt

foreign import ccall unsafe "HaskellPosix.h recvmmsg_sockaddr_discard"
  c_unsafe_recvmmsg_sockaddr_discard ::
       Fd
    -> MutableByteArray# RealWorld -- lengths
    -> MutableArray# RealWorld (MutableByteArray# RealWorld) -- buffers
    -> CUInt -- Length of msghdr array
    -> MessageFlags 'Receive
    -> IO CInt
#endif

{- | Linux extends the @type@ argument of
  <http://man7.org/linux/man-pages/man2/socket.2.html socket> to allow
  setting two socket flags on socket creation: @SOCK_CLOEXEC@ and
  @SOCK_NONBLOCK@. It is advisable to set @SOCK_CLOEXEC@ on when
  opening a socket on linux. For example, we may open a TCP Internet
  socket with:

  > uninterruptibleSocket internet (applySocketFlags closeOnExec stream) defaultProtocol

  To additionally open the socket in nonblocking mode
  (e.g. with @SOCK_NONBLOCK@):

  > uninterruptibleSocket internet (applySocketFlags (closeOnExec <> nonblocking) stream) defaultProtocol
-}
applySocketFlags :: SocketFlags -> Type -> Type
applySocketFlags (SocketFlags s) (Type t) = Type (s .|. t)

#if defined(UNLIFTEDARRAYFUNCTIONS)
-- | Receive multiple messages. This does not provide the socket
--   addresses or the control messages. It does not use any of the
--   input-scattering that @recvmmsg@ offers, meaning that a single
--   datagram is never split across noncontiguous memory. It supplies
--   @NULL@ for the timeout argument. All of the messages must have the
--   same maximum size. All resulting byte arrays have been explicitly
--   pinned. In addition to bytearrays corresponding to each datagram,
--   this also provides the maximum @msg_len@ that @recvmmsg@ wrote
--   back out. This is provided so that users of @MSG_TRUNC@ can detect
--   when bytes were dropped from the end of a message (although it does
--   let the user figure out which message had bytes dropped).
uninterruptibleReceiveMultipleMessageA ::
     Fd -- ^ Socket
  -> CSize -- ^ Maximum bytes per message
  -> CUInt -- ^ Maximum number of messages
  -> MessageFlags 'Receive -- ^ Flags
  -> IO (Either Errno (CUInt,UnliftedArray ByteArray))
uninterruptibleReceiveMultipleMessageA !s !msgSize !msgCount !flags = do
  placeholder <- PM.newByteArray 0
  bufs <- PM.newUnliftedArray (cuintToInt msgCount) placeholder
  mmsghdrsBuf <- PM.newPinnedByteArray (cuintToInt msgCount * cintToInt LST.sizeofMultipleMessageHeader)
  iovecsBuf <- PM.newPinnedByteArray (cuintToInt msgCount * cintToInt S.sizeofIOVector)
  let !mmsghdrsAddr@(Addr mmsghdrsAddr#) = ptrToAddr (PM.mutableByteArrayContents mmsghdrsBuf)
  let iovecsAddr = ptrToAddr (PM.mutableByteArrayContents iovecsBuf)
  initializeMultipleMessageHeadersWithoutSockAddr bufs iovecsAddr mmsghdrsAddr msgSize msgCount
  r <- c_unsafe_addr_recvmmsg s mmsghdrsAddr# msgCount flags nullAddr#
  if r > (-1)
    then do
      (_,maxMsgSz,frozenBufs) <- shrinkAndFreezeMessages msgSize 0 (cssizeToInt r) bufs mmsghdrsAddr
      touchMutableUnliftedArray bufs
      touchMutableByteArray iovecsBuf
      touchMutableByteArray mmsghdrsBuf
      pure (Right (maxMsgSz,frozenBufs))
    else do
      touchMutableUnliftedArray bufs
      touchMutableByteArray iovecsBuf
      touchMutableByteArray mmsghdrsBuf
      fmap Left getErrno

-- | Receive multiple messages. This is similar to
-- @uninterruptibleReceiveMultipleMessageA@. However, it also
-- provides the @sockaddr@s of the remote endpoints. These are
-- written in contiguous memory to a bytearray of length
-- @max_num_msgs * expected_sockaddr_sz@. The @sockaddr@s must
-- all be expected to be of the same length. This function
-- provides a @sockaddr@ size check that is non-zero when any
-- @sockaddr@ had a length other than the expected length.
-- This can be used to detect if the @sockaddr@ array has one or
-- more corrupt @sockaddr@s in it. All byte arrays returned by
-- this function are pinned.
--
-- The values in the returned tuple are:
--
-- * Error-checking number for @sockaddr@ size. Non-zero indicates
--   that at least one @sockaddr@ required a number of bytes other
--   than the expected number.
-- * Pinned bytearray with all of the @sockaddr@s in it as a
--   array of structures.
-- * The size of the largest message received. If @MSG_TRUNC@ is used
--   this lets the caller know if one or more messages were truncated.
-- * The message data of each message.
--
-- The @sockaddr@s bytearray and the unlifted array of messages are
-- guaranteed to have the same number of elements.
uninterruptibleReceiveMultipleMessageB ::
     Fd -- ^ Socket
  -> CInt -- ^ Expected @sockaddr@ size
  -> CSize -- ^ Maximum bytes per message
  -> CUInt -- ^ Maximum number of messages
  -> MessageFlags 'Receive -- ^ Flags
  -> IO (Either Errno (CInt,ByteArray,CUInt,UnliftedArray ByteArray))
uninterruptibleReceiveMultipleMessageB !s !expSockAddrSize !msgSize !msgCount !flags = do
  placeholder <- PM.newByteArray 0
  bufs <- PM.newUnliftedArray (cuintToInt msgCount) placeholder
  mmsghdrsBuf <- PM.newPinnedByteArray (cuintToInt msgCount * cintToInt LST.sizeofMultipleMessageHeader)
  iovecsBuf <- PM.newPinnedByteArray (cuintToInt msgCount * cintToInt S.sizeofIOVector)
  sockaddrsBuf <- PM.newPinnedByteArray (cuintToInt msgCount * cintToInt expSockAddrSize)
  -- Linux does not require zeroing out sockaddr_in before using it,
  -- so we leave sockaddrsBuf alone after initialization.
  let sockaddrsAddr = ptrToAddr (PM.mutableByteArrayContents sockaddrsBuf)
  let !mmsghdrsAddr@(Addr mmsghdrsAddr#) = ptrToAddr (PM.mutableByteArrayContents mmsghdrsBuf)
  let iovecsAddr = ptrToAddr (PM.mutableByteArrayContents iovecsBuf)
  initializeMultipleMessageHeadersWithSockAddr bufs iovecsAddr mmsghdrsAddr sockaddrsAddr expSockAddrSize msgSize msgCount
  r <- c_unsafe_addr_recvmmsg s mmsghdrsAddr# msgCount flags nullAddr#
  if r > (-1)
    then do
      (validation,maxMsgSz,frozenBufs) <- shrinkAndFreezeMessages msgSize expSockAddrSize (cssizeToInt r) bufs mmsghdrsAddr
      shrinkMutableByteArray sockaddrsBuf (cssizeToInt r * cintToInt expSockAddrSize)
      sockaddrs <- PM.unsafeFreezeByteArray sockaddrsBuf
      touchMutableByteArray iovecsBuf
      touchMutableByteArray mmsghdrsBuf
      touchMutableByteArray sockaddrsBuf
      pure (Right (validation,sockaddrs,maxMsgSz,frozenBufs))
    else do
      touchMutableUnliftedArray bufs
      touchMutableByteArray iovecsBuf
      touchMutableByteArray mmsghdrsBuf
      touchMutableByteArray sockaddrsBuf
      fmap Left getErrno

-- | All three buffer arguments need to have the same length (in elements, not bytes).
uninterruptibleReceiveMultipleMessageC ::
     Fd -- ^ Socket
  -> MutablePrimArray RealWorld CInt -- ^ Buffer for payload lengths
  -> MutablePrimArray RealWorld S.SocketAddressInternet -- ^ Buffer for @sockaddr_in@s
  -> MutableUnliftedArray RealWorld (MutableByteArray RealWorld) -- ^ Buffers for payloads
  -> CUInt -- ^ Maximum number of datagrams to receive, length of buffers
  -> MessageFlags 'Receive -- ^ Flags
  -> IO (Either Errno CInt)
uninterruptibleReceiveMultipleMessageC !s (MutablePrimArray lens) (MutablePrimArray addrs) (MutableUnliftedArray (MutableUnliftedArray# payloads)) !msgCount !flags =
  c_unsafe_recvmmsg_sockaddr_in s lens addrs payloads msgCount flags >>= errorsFromInt

-- | All three buffer arguments need to have the same length (in elements, not bytes).
-- This discards the source addresses.
uninterruptibleReceiveMultipleMessageD ::
     Fd -- ^ Socket
  -> MutablePrimArray RealWorld CInt -- ^ Buffer for payload lengths
  -> MutableUnliftedArray RealWorld (MutableByteArray RealWorld) -- ^ Buffers for payloads
  -> CUInt -- ^ Maximum number of datagrams to receive, length of buffers
  -> MessageFlags 'Receive -- ^ Flags
  -> IO (Either Errno CInt)
uninterruptibleReceiveMultipleMessageD !s (MutablePrimArray lens) (MutableUnliftedArray (MutableUnliftedArray# payloads)) !msgCount !flags =
  c_unsafe_recvmmsg_sockaddr_discard s lens payloads msgCount flags >>= errorsFromInt

-- This sets up an array of mmsghdr. Each msghdr has msg_iov set to
-- be an array of iovec with a single element.
initializeMultipleMessageHeadersWithoutSockAddr ::
     MutableUnliftedArray RealWorld (MutableByteArray RealWorld) -- buffers
  -> Addr -- array of iovec
  -> Addr -- array of message headers
  -> CSize -- message size
  -> CUInt -- message count
  -> IO ()
initializeMultipleMessageHeadersWithoutSockAddr bufs iovecsAddr mmsgHdrsAddr msgSize msgCount =
  let go !ix !iovecAddr !mmsgHdrAddr = if ix < cuintToInt msgCount
        then do
          pokeMultipleMessageHeader mmsgHdrAddr nullAddr 0 iovecAddr 1 nullAddr 0 mempty 0
          initializeIOVector bufs iovecAddr msgSize ix
          go (ix + 1) (plusAddr iovecAddr (cintToInt S.sizeofIOVector)) (plusAddr mmsgHdrAddr (cintToInt LST.sizeofMultipleMessageHeader))
        else pure ()
   in go 0 iovecsAddr mmsgHdrsAddr

-- This sets up an array of mmsghdr. Each msghdr has msg_iov set to
-- be an array of iovec with a single element. One giant buffer with
-- space for all of the @sockaddr@s is used.
initializeMultipleMessageHeadersWithSockAddr ::
     MutableUnliftedArray RealWorld (MutableByteArray RealWorld)
  -> Addr -- array of iovec
  -> Addr -- array of message headers
  -> Addr -- array of sockaddrs
  -> CInt -- expected sockaddr size
  -> CSize -- message size
  -> CUInt -- message count
  -> IO ()
initializeMultipleMessageHeadersWithSockAddr bufs iovecsAddr0 mmsgHdrsAddr0 sockaddrsAddr0 sockaddrSize msgSize msgCount =
  let go !ix !iovecAddr !mmsgHdrAddr !sockaddrAddr = if ix < cuintToInt msgCount
        then do
          pokeMultipleMessageHeader mmsgHdrAddr sockaddrAddr sockaddrSize iovecAddr 1 nullAddr 0 mempty 0
          initializeIOVector bufs iovecAddr msgSize ix
          go (ix + 1)
            (plusAddr iovecAddr (cintToInt S.sizeofIOVector))
            (plusAddr mmsgHdrAddr (cintToInt LST.sizeofMultipleMessageHeader))
            (plusAddr sockaddrAddr (cintToInt sockaddrSize))
        else pure ()
   in go 0 iovecsAddr0 mmsgHdrsAddr0 sockaddrsAddr0

ptrToAddr :: Ptr Word8 -> Addr
ptrToAddr (Ptr x) = Addr x

-- Initialize a single iovec. We write the pinned byte array into
-- both the iov_base field and into an unlifted array.
initializeIOVector ::
     MutableUnliftedArray RealWorld (MutableByteArray RealWorld)
  -> Addr
  -> CSize
  -> Int
  -> IO ()
initializeIOVector bufs iovecAddr msgSize ix = do
  buf <- PM.newPinnedByteArray (csizeToInt msgSize)
  PM.writeUnliftedArray bufs ix buf
  S.pokeIOVectorBase iovecAddr (ptrToAddr (PM.mutableByteArrayContents buf))
  S.pokeIOVectorLength iovecAddr msgSize

-- Freeze a slice of the mutable byte arrays inside the unlifted array,
-- shrinking the byte arrays before doing so.
shrinkAndFreezeMessages ::
     CSize -- Full size of each buffer
  -> CInt -- Expected sockaddr size
  -> Int -- Actual number of received messages
  -> MutableUnliftedArray RealWorld (MutableByteArray RealWorld)
  -> Addr -- Array of mmsghdr
  -> IO (CInt,CUInt,UnliftedArray ByteArray)
shrinkAndFreezeMessages !bufSize !expSockAddrSize !n !bufs !mmsghdr0 = do
  r <- PM.unsafeNewUnliftedArray n
  go r 0 0 0 mmsghdr0
  where
  go !r !validation !ix !maxMsgSz !mmsghdr = if ix < n
    then do
      sz <- LST.peekMultipleMessageHeaderLength mmsghdr
      sockaddrSz <- LST.peekMultipleMessageHeaderNameLength mmsghdr
      buf <- PM.readUnliftedArray bufs ix
      when (cuintToInt sz < csizeToInt bufSize) (shrinkMutableByteArray buf (cuintToInt sz))
      PM.writeUnliftedArray r ix =<< PM.unsafeFreezeByteArray buf
      go r (validation .|. (sockaddrSz - expSockAddrSize)) (ix + 1) (max maxMsgSz sz)
        (plusAddr mmsghdr (cintToInt LST.sizeofMultipleMessageHeader))
    else do
      a <- PM.unsafeFreezeUnliftedArray r
      pure (validation,maxMsgSz,a)
#endif

-- 2024-02-05: Commented out unused function.
-- pokeMultipleMessageHeader :: Addr -> Addr -> CInt -> Addr -> CSize -> Addr -> CSize -> MessageFlags 'Receive -> CUInt -> IO ()
-- pokeMultipleMessageHeader mmsgHdrAddr a b c d e f g len = do
--   LST.pokeMultipleMessageHeaderName mmsgHdrAddr a
--   LST.pokeMultipleMessageHeaderNameLength mmsgHdrAddr b
--   LST.pokeMultipleMessageHeaderIOVector mmsgHdrAddr c
--   LST.pokeMultipleMessageHeaderIOVectorLength mmsgHdrAddr d
--   LST.pokeMultipleMessageHeaderControl mmsgHdrAddr e
--   LST.pokeMultipleMessageHeaderControlLength mmsgHdrAddr f
--   LST.pokeMultipleMessageHeaderFlags mmsgHdrAddr g
--   LST.pokeMultipleMessageHeaderLength mmsgHdrAddr len

shrinkMutableByteArray :: MutableByteArray RealWorld -> Int -> IO ()
shrinkMutableByteArray (MutableByteArray arr) (I# sz) =
  PM.primitive_ (shrinkMutableByteArray# arr sz)

{- | Variant of 'Posix.Socket.uninterruptibleAccept' that allows setting
  flags on the newly-accepted connection.
-}
uninterruptibleAccept4 ::
  -- | Listening socket
  Fd ->
  -- | Maximum socket address size
  CInt ->
  -- | Set non-blocking and close-on-exec without extra syscall
  SocketFlags ->
  -- | Peer information and connected socket
  IO (Either Errno (CInt, SocketAddress, Fd))
{-# INLINE uninterruptibleAccept4 #-}
uninterruptibleAccept4 !sock !maxSz !flags = do
  sockAddrBuf@(MutableByteArray sockAddrBuf#) <- PM.newByteArray (cintToInt maxSz)
  lenBuf@(MutableByteArray lenBuf#) <- PM.newByteArray (PM.sizeOf (undefined :: CInt))
  PM.writeByteArray lenBuf 0 maxSz
  r <- c_unsafe_accept4 sock sockAddrBuf# lenBuf# flags
  if r > (-1)
    then do
      (sz :: CInt) <- PM.readByteArray lenBuf 0
      if sz < maxSz
        then shrinkMutableByteArray sockAddrBuf (cintToInt sz)
        else pure ()
      sockAddr <- PM.unsafeFreezeByteArray sockAddrBuf
      pure (Right (sz, SocketAddress sockAddr, r))
    else fmap Left getErrno

{- | Variant of 'uninterruptibleAccept4' that requests that the kernel not
include the socket address in its reponse.
-}
uninterruptibleAccept4_ ::
  -- | Listening socket
  Fd ->
  -- | Set non-blocking and close-on-exec without extra syscall
  SocketFlags ->
  -- | Connected socket
  IO (Either Errno Fd)
{-# INLINE uninterruptibleAccept4_ #-}
uninterruptibleAccept4_ !sock !flags = do
  r <- c_unsafe_ptr_accept4 sock nullPtr nullPtr flags
  if r > (-1)
    then pure (Right r)
    else fmap Left getErrno

cintToInt :: CInt -> Int
cintToInt = fromIntegral

-- 2024-02-05: Commented out unused functions.
-- cuintToInt :: CUInt -> Int
-- cuintToInt = fromIntegral

-- csizeToInt :: CSize -> Int
-- csizeToInt = fromIntegral

-- cssizeToInt :: CSsize -> Int
-- cssizeToInt = fromIntegral

-- errorsFromInt :: CInt -> IO (Either Errno CInt)
-- {-# inline errorsFromInt #-}
-- errorsFromInt r = if r > (-1)
--   then pure (Right r)
--   else fmap Left getErrno

-- touchMutableByteArray :: MutableByteArray RealWorld -> IO ()
-- touchMutableByteArray (MutableByteArray x) = touchMutableByteArray# x

-- touchMutableByteArray# :: MutableByteArray# RealWorld -> IO ()
-- touchMutableByteArray# x = IO $ \s -> case touch# x s of s' -> (# s', () #)

#if defined(UNLIFTEDARRAYFUNCTIONS)
touchMutableUnliftedArray :: MutableUnliftedArray RealWorld a -> IO ()
touchMutableUnliftedArray (MutableUnliftedArray x) = touchMutableUnliftedArray# x

touchMutableUnliftedArray# :: MutableUnliftedArray# RealWorld a -> IO ()
touchMutableUnliftedArray# x = IO $ \s -> case touch# x s of s' -> (# s', () #)
#endif
