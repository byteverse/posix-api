{-# language BangPatterns #-}
{-# language DerivingStrategies #-}
{-# language DuplicateRecordFields #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language MagicHash #-}
{-# language NamedFieldPuns #-}
{-# language UnboxedTuples #-}
{-# language ScopedTypeVariables #-}

#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netinet/ip.h>
#include <arpa/inet.h>

-- | All of the data constructors provided by this module are unsafe.
--   Only use them if you really know what you are doing.
module Posix.Socket.Platform
  ( -- * Encoding Socket Addresses
    encodeSocketAddressInternet
  , encodeSocketAddressUnix
  ) where

import Data.Primitive (ByteArray(..),Addr(..))
import Data.Void (Void)
import Data.Word (Word8)
import Foreign.C.Types (CUShort)
import Foreign.Storable (pokeByteOff)
import GHC.Exts (ByteArray##,State##,RealWorld,Ptr(..),runRW##,touch##)
import GHC.IO (IO(..))
import Posix.Socket.Types (SocketAddressInternet(..),SocketAddressUnix(..))
import Posix.Socket.Types (SocketAddress(..))
import Control.Monad (when)

import qualified Data.Primitive as PM
import qualified Foreign.Storable as FS

-- | Serialize a IPv4 socket address so that it may be passed to @bind@.
--   This serialization is operating-system dependent.
encodeSocketAddressInternet :: SocketAddressInternet -> SocketAddress
encodeSocketAddressInternet (SocketAddressInternet {port, address}) =
  SocketAddress $ runByteArrayIO $ unboxByteArrayIO $ do
    bs <- PM.newPinnedByteArray #{size struct sockaddr_in}
    -- Initialize the bytearray by filling it with zeroes to ensure
    -- that the sin_zero padding that linux expects is properly zeroed.
    PM.setByteArray bs 0 #{size struct sockaddr_in} (0 :: Word8)
    let !(Addr addr) = PM.mutableByteArrayContents bs
    let !(ptr :: Ptr Void) = Ptr addr
    -- ATM: I cannot find a way to poke AF_INET into the socket address
    -- without hardcoding the expected length (CUShort). There may be
    -- a way to use hsc2hs to convert a size to a haskell type, but
    -- I am not sure of how to do this. At any rate, I do not expect
    -- that linux will ever change the bit size of sa_family_t, so I
    -- am not too concerned.
    #{poke struct sockaddr_in, sin_family} ptr (#{const AF_INET} :: CUShort)
    -- The port and the address are already supposed to be in network
    -- byte order in the SocketAddressInternet data type.
    #{poke struct sockaddr_in, sin_port} ptr port
    #{poke struct sockaddr_in, sin_addr.s_addr} ptr address
    r <- PM.unsafeFreezeByteArray bs
    touchByteArray r
    pure r

-- | Serialize a unix domain socket address so that it may be passed to @bind@.
--   This serialization is operating-system dependent. If the path provided by
--   the argument equals or exceeds the size of @sun_path@ (typically in the range 92
--   to 108 but varies by platform), the socket address will instead be given the
--   empty string as its path. This typically results in @bind@ returning an
--   error code.
encodeSocketAddressUnix :: SocketAddressUnix -> SocketAddress
encodeSocketAddressUnix (SocketAddressUnix !name) =
  SocketAddress $ runByteArrayIO $ unboxByteArrayIO $ do 
    -- On linux, sun_path always has exactly 108 bytes. It is a null-terminated
    -- string, so we initialize the byte array to zeroes to ensure this
    -- happens.
    let pathSize = 108 :: Int
    -- Again, we hard-code the size of sa_family_t as the size of
    -- an unsigned short.
    let familySize = FS.sizeOf (undefined :: CUShort)
    bs <- PM.newPinnedByteArray (pathSize + familySize)
    PM.setByteArray bs familySize pathSize (0 :: Word8)
    PM.writeByteArray bs 0 (#{const AF_UNIX} :: CUShort)
    let sz = PM.sizeofByteArray name
    when (sz < pathSize) $ do
      PM.copyByteArray bs familySize name 0 sz
    PM.unsafeFreezeByteArray bs

touchByteArray :: ByteArray -> IO ()
touchByteArray (ByteArray x) = touchByteArray## x

touchByteArray## :: ByteArray## -> IO ()
touchByteArray## x = IO $ \s -> case touch## x s of s' -> (## s', () ##)

unboxByteArrayIO :: IO ByteArray -> State## RealWorld -> (## State## RealWorld, ByteArray## ##)
unboxByteArrayIO (IO f) s = case f s of
  (## s', ByteArray b ##) -> (## s', b ##)

-- In a general setting, this function is unsafe, but it is used kind of like
-- runST in this module.
runByteArrayIO :: (State## RealWorld -> (## State## RealWorld, ByteArray## ##)) -> ByteArray
runByteArrayIO st_rep = case runRW## st_rep of (## _, a ##) -> ByteArray a

