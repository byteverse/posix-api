{-# language BangPatterns #-}
{-# language DerivingStrategies #-}
{-# language DuplicateRecordFields #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language MagicHash #-}
{-# language UnboxedTuples #-}
{-# language ScopedTypeVariables #-}

#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netinet/ip.h>
#include <arpa/inet.h>

-- | All of the data constructors provided by this module are unsafe.
--   Only use them if you really know what you are doing.
module Linux.Socket.Types
  ( -- * Encoding Socket Addresses
    encodeSocketAddressInternet
  , encodeSocketAddressUnix
    -- * Socket Types
  , raw
  ) where

import Data.Primitive (ByteArray(..),Addr(..))
import Data.Void (Void)
import Data.Word (Word8)
import Foreign.C.Types (CUShort)
import Foreign.Storable (pokeByteOff)
import GHC.Exts (ByteArray##,State##,RealWorld,Ptr(..),runRW##,touch##)
import GHC.IO (IO(..))
import Posix.Socket (SocketAddressInternet(..),SocketAddressUnix(..),SocketAddress(..),Type(..))

import qualified Data.Primitive as PM
import qualified Foreign.Storable as FS

encodeSocketAddressInternet :: SocketAddressInternet -> SocketAddress
encodeSocketAddressInternet (SocketAddressInternet netPort netAddr) =
  SocketAddress $ runByteArrayIO $ unboxByteArrayIO $ do
    bs <- PM.newPinnedByteArray #{size struct sockaddr}
    -- Initialize the bytearray by filling it with zeroes to ensure
    -- that the sin_zero padding that linux expects is properly zeroed.
    -- Notice that the size of the byte array is the size of sockaddr,
    -- not the size of sockaddr_in.
    PM.setByteArray bs 0 #{size struct sockaddr} (0 :: Word8)
    let !(Addr addr) = PM.mutableByteArrayContents bs
    let !(ptr :: Ptr Void) = Ptr addr
    -- ATM: I cannot find a way to poke AF_INET into the socket address
    -- without hardcoding the expected length (CUShort). There may be
    -- a way to use hsc2hs to convert a size to a haskell type, but
    -- I am not sure of how to do this. At any rate, I do not expect
    -- that linux will ever change to bit size of sa_family_t, so I
    -- am not too concerned.
    #{poke struct sockaddr_in, sin_family} ptr (#{const AF_INET} :: CUShort)
    -- TODO: the port and the address are supposed to be in network
    -- byte order. Figure out where we want this conversion to
    -- take place. I think that "in the data type" is the right
    -- answer though.
    #{poke struct sockaddr_in, sin_port} ptr netPort
    #{poke struct sockaddr_in, sin_addr} ptr netAddr
    r <- PM.unsafeFreezeByteArray bs
    touchByteArray r
    pure r

encodeSocketAddressUnix :: SocketAddressUnix -> SocketAddress
encodeSocketAddressUnix (SocketAddressUnix !name) =
  SocketAddress $ runByteArrayIO $ unboxByteArrayIO $ do 
    let sz = PM.sizeofByteArray name
    -- Again, we hard-code the size of sa_family_t. Disappointing.
    bs <- PM.newPinnedByteArray (sz + FS.sizeOf (undefined :: CUShort))
    PM.writeByteArray bs 0 (#{const AF_INET} :: CUShort)
    PM.copyByteArray bs (FS.sizeOf (undefined :: CUShort)) name 0 sz
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

-- | The @SOCK_RAW@ socket type.
raw :: Type
raw = Type #{const SOCK_RAW}


