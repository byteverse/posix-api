{-# language BangPatterns #-}
{-# language DataKinds #-}
{-# language MagicHash #-}
{-# language ScopedTypeVariables #-}
{-# language UnboxedTuples #-}
{-# language UnliftedFFITypes #-}

module Linux.Socket
  ( -- * Functions
    uninterruptibleReceiveMultipleMessageA
    -- * Types
  , SocketFlags(..)
    -- * Message Flags
  , LST.dontWait
  , LST.truncate
  , LST.controlTruncate
    -- * Socket Flags
  , LST.closeOnExec
  , LST.nonblocking
    -- * Twiddle
  , applySocketFlags
  ) where

import Prelude hiding (truncate)

import Control.Monad (when)
import Data.Bits ((.|.))
import Data.Primitive (MutablePrimArray(..),MutableByteArray(..),Addr(..),ByteArray(..))
import Data.Primitive (MutableUnliftedArray(..),UnliftedArray)
import Foreign.C.Error (Errno,getErrno)
import Foreign.C.Types (CInt(..),CSize(..),CUInt(..))
import GHC.Exts (Ptr,RealWorld,ByteArray#,MutableByteArray#,Addr#,MutableArrayArray#,Int(I#))
import GHC.Exts (shrinkMutableByteArray#,touch#,nullAddr#)
import GHC.IO (IO(..))
import Linux.Socket.Types (SocketFlags(..))
import Posix.Socket (Type(..),MessageFlags(..),Message(Receive))
import System.Posix.Types (Fd(..),CSsize(..))

import qualified Data.Primitive as PM
import qualified Control.Monad.Primitive as PM
import qualified Posix.Socket as S
import qualified Linux.Socket.Types as LST

foreign import ccall unsafe "sys/socket.h recvmmsg"
  c_unsafe_addr_recvmmsg :: Fd
                         -> Addr# -- This addr is an array of msghdr
                         -> CUInt -- Length of msghdr array
                         -> MessageFlags 'Receive
                         -> Addr# -- Timeout
                         -> IO CSsize

-- | Linux extends the @type@ argument of
--   <http://man7.org/linux/man-pages/man2/socket.2.html socket> to accept
--   flags. It is advisable to set @SOCK_CLOEXEC@ on when opening a socket
--   on linux. For example, we may open a TCP Internet socket with:
--
--   > uninterruptibleSocket internet (applySocketFlags closeOnExec stream) defaultProtocol
--
--   To additionally open the socket in nonblocking mode
--   (e.g. with @SOCK_NONBLOCK@):
--
--   > uninterruptibleSocket internet (applySocketFlags (closeOnExec <> nonblocking) stream) defaultProtocol
--   
applySocketFlags :: SocketFlags -> Type -> Type
applySocketFlags (SocketFlags s) (Type t) = Type (s .|. t)

-- | Receive multiple messages. This does not provide the socket
--   addresses or the control messages. It supplies @NULL@ for the
--   timeout argument. All of the messages must have the same maximum
--   size. All resulting byte arrays have been explicitly pinned.
uninterruptibleReceiveMultipleMessageA ::
     Fd -- ^ Socket
  -> CSize -- ^ Maximum bytes per message
  -> CUInt -- ^ Maximum number of messages
  -> MessageFlags 'Receive -- ^ Flags
  -> IO (Either Errno (CUInt,UnliftedArray ByteArray))
uninterruptibleReceiveMultipleMessageA !s !msgSize !msgCount !flags = do
  bufs <- PM.unsafeNewUnliftedArray (cuintToInt msgCount)
  mmsghdrsBuf <- PM.newPinnedByteArray (cuintToInt msgCount * cintToInt LST.sizeofMultipleMessageHeader)
  iovecsBuf <- PM.newPinnedByteArray (cuintToInt msgCount * cintToInt S.sizeofIOVector)
  let !mmsghdrsAddr@(Addr mmsghdrsAddr#) = PM.mutableByteArrayContents mmsghdrsBuf
  let iovecsAddr = PM.mutableByteArrayContents iovecsBuf
  initializeMultipleMessageHeadersWithoutSockAddr bufs iovecsAddr mmsghdrsAddr msgSize msgCount
  r <- c_unsafe_addr_recvmmsg s mmsghdrsAddr# msgCount flags nullAddr#
  if r > (-1)
    then do
      (maxMsgSz,frozenBufs) <- shrinkAndFreezeMessages msgSize (cssizeToInt r) bufs mmsghdrsAddr
      touchMutableUnliftedArray bufs
      touchMutableByteArray iovecsBuf
      touchMutableByteArray mmsghdrsBuf
      pure (Right (maxMsgSz,frozenBufs))
    else do
      touchMutableUnliftedArray bufs
      touchMutableByteArray iovecsBuf
      touchMutableByteArray mmsghdrsBuf
      fmap Left getErrno

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
          -- fail ("uhoetuh: " ++ show iovecsAddr ++ " " ++ show msgSize ++ " " ++ show msgCount)
          pokeMultipleMessageHeader mmsgHdrAddr PM.nullAddr 0 iovecAddr 1 PM.nullAddr 0 mempty 0
          initializeIOVector bufs iovecAddr msgSize ix
          go (ix + 1) (PM.plusAddr iovecAddr (cintToInt S.sizeofIOVector)) (PM.plusAddr mmsgHdrAddr (cintToInt LST.sizeofMultipleMessageHeader))
        else pure ()
   in go 0 iovecsAddr mmsgHdrsAddr

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
  S.pokeIOVectorBase iovecAddr (PM.mutableByteArrayContents buf)
  S.pokeIOVectorLength iovecAddr msgSize

-- Freeze a slice of the mutable byte arrays inside the unlifted array,
-- shrinking the byte arrays before doing so.
shrinkAndFreezeMessages ::
     CSize -- Full size of each buffer
  -> Int -- Actual number of received messages
  -> MutableUnliftedArray RealWorld (MutableByteArray RealWorld)
  -> Addr -- Array of mmsghdr
  -> IO (CUInt,UnliftedArray ByteArray)
shrinkAndFreezeMessages !bufSize !n !bufs !mmsghdr0 = do
  r <- PM.unsafeNewUnliftedArray n
  go r 0 0 mmsghdr0
  where
  go !r !ix !maxMsgSz !mmsghdr = if ix < n
    then do
      sz <- LST.peekMultipleMessageHeaderLength mmsghdr
      buf <- PM.readUnliftedArray bufs ix
      when (cuintToInt sz < csizeToInt bufSize) (shrinkMutableByteArray buf (cuintToInt sz))
      PM.writeUnliftedArray r ix =<< PM.unsafeFreezeByteArray buf
      go r (ix + 1) (max maxMsgSz sz)
        (PM.plusAddr mmsghdr (cintToInt LST.sizeofMultipleMessageHeader))
    else do
      a <- PM.unsafeFreezeUnliftedArray r
      pure (maxMsgSz,a)

pokeMultipleMessageHeader :: Addr -> Addr -> CInt -> Addr -> CSize -> Addr -> CSize -> MessageFlags 'Receive -> CUInt -> IO ()
pokeMultipleMessageHeader mmsgHdrAddr a b c d e f g len = do
  LST.pokeMultipleMessageHeaderName mmsgHdrAddr a
  LST.pokeMultipleMessageHeaderNameLength mmsgHdrAddr b
  LST.pokeMultipleMessageHeaderIOVector mmsgHdrAddr c
  LST.pokeMultipleMessageHeaderIOVectorLength mmsgHdrAddr d
  LST.pokeMultipleMessageHeaderControl mmsgHdrAddr e
  LST.pokeMultipleMessageHeaderControlLength mmsgHdrAddr f
  LST.pokeMultipleMessageHeaderFlags mmsgHdrAddr g
  LST.pokeMultipleMessageHeaderLength mmsgHdrAddr len

shrinkMutableByteArray :: MutableByteArray RealWorld -> Int -> IO ()
shrinkMutableByteArray (MutableByteArray arr) (I# sz) =
  PM.primitive_ (shrinkMutableByteArray# arr sz)

cintToInt :: CInt -> Int
cintToInt = fromIntegral

cuintToInt :: CUInt -> Int
cuintToInt = fromIntegral

csizeToInt :: CSize -> Int
csizeToInt = fromIntegral

cssizeToInt :: CSsize -> Int
cssizeToInt = fromIntegral

touchMutableUnliftedArray :: MutableUnliftedArray RealWorld a -> IO ()
touchMutableUnliftedArray (MutableUnliftedArray x) = touchMutableUnliftedArray# x

touchMutableByteArray :: MutableByteArray RealWorld -> IO ()
touchMutableByteArray (MutableByteArray x) = touchMutableByteArray# x

touchMutableUnliftedArray# :: MutableArrayArray# RealWorld -> IO ()
touchMutableUnliftedArray# x = IO $ \s -> case touch# x s of s' -> (# s', () #)

touchMutableByteArray# :: MutableByteArray# RealWorld -> IO ()
touchMutableByteArray# x = IO $ \s -> case touch# x s of s' -> (# s', () #)
