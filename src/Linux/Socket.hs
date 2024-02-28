{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnliftedFFITypes #-}

module Linux.Socket
  ( -- * Functions
    uninterruptibleAccept4
  , uninterruptibleAccept4_

    -- * Types
  , SocketFlags (..)

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

import Prelude hiding (truncate)

import Data.Bits ((.|.))
import Data.Primitive (MutableByteArray (..))
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
import qualified Linux.Socket.Types as LST

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
