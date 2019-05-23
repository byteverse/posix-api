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
#include "custom.h"

module Posix.Socket.Platform
  ( -- * Encoding Socket Addresses
    encodeSocketAddressInternet
  , encodeSocketAddressUnix
    -- * Decoding Socket Addresses
  , decodeSocketAddressInternet
  , indexSocketAddressInternet
    -- * Sizes
  , sizeofSocketAddressInternet
  ) where

import Control.Monad (when)
import Data.Primitive (MutableByteArray,ByteArray(..),writeByteArray,indexByteArray)
import Data.Primitive.Addr (Addr(..))
import Data.Word (Word8)
import Foreign.C.Types (CUShort,CInt)
import GHC.Exts (ByteArray##,State##,RealWorld,runRW##,Ptr(..))
import GHC.ST (ST(..))
import Posix.Socket.Types (SocketAddress(..))
import Posix.Socket.Types (SocketAddressInternet(..),SocketAddressUnix(..))
import Foreign.Storable (peekByteOff)

import qualified Data.Primitive as PM
import qualified Data.Primitive.Addr as PMA
import qualified Foreign.Storable as FS

-- | The size of a serialized internet socket address.  
sizeofSocketAddressInternet :: CInt
sizeofSocketAddressInternet = #{size struct sockaddr_in}

internalWriteSocketAddressInternet ::
     MutableByteArray s -- ^ Buffer, must have length of @sockaddr_in@
  -> SocketAddressInternet
  -> ST s ()
internalWriteSocketAddressInternet bs (SocketAddressInternet {port, address}) = do
  -- Initialize the bytearray by filling it with zeroes to ensure
  -- that the sin_zero padding that linux expects is properly zeroed.
  PM.setByteArray bs 0 #{size struct sockaddr_in} (0 :: Word8)
  -- ATM: I cannot find a way to poke AF_INET into the socket address
  -- without hardcoding the expected length (CUShort). There may be
  -- a way to use hsc2hs to convert a size to a haskell type, but
  -- I am not sure of how to do this. At any rate, I do not expect
  -- that linux will ever change the bit size of sa_family_t, so I
  -- am not too concerned.
  #{writeByteArray struct sockaddr_in, sin_family} bs 0 (#{const AF_INET} :: CUShort)
  -- The port and the address are already supposed to be in network
  -- byte order in the SocketAddressInternet data type.
  #{writeByteArray struct sockaddr_in, sin_port} bs 0 port
  #{writeByteArray struct sockaddr_in, sin_addr.s_addr} bs 0 address

-- | Serialize a IPv4 socket address so that it may be passed to @bind@.
--   This serialization is operating-system dependent.
encodeSocketAddressInternet :: SocketAddressInternet -> SocketAddress
encodeSocketAddressInternet sockAddrInternet =
  SocketAddress $ runByteArrayST $ unboxByteArrayST $ do
    bs <- PM.newByteArray #{size struct sockaddr_in}
    internalWriteSocketAddressInternet bs sockAddrInternet
    r <- PM.unsafeFreezeByteArray bs
    pure r

-- | Decode a @sockaddr_in@ from a @sockaddr@ of an unknown
--   family. This returns nothing when the size of the @sockaddr@
--   is wrong or when the @sin_family@ is not @AF_INET@.
decodeSocketAddressInternet :: SocketAddress -> Maybe SocketAddressInternet
decodeSocketAddressInternet (SocketAddress arr) =
  if PM.sizeofByteArray arr == (#{size struct sockaddr_in})
    -- We assume that AF_INET takes up 16 bits. See the comment in
    -- encodeSocketAddressInternet for more detail.
    then if (#{indexByteArray struct sockaddr_in, sin_family} arr 0) == (#{const AF_INET} :: CUShort)
      then Just $ SocketAddressInternet
        { port = #{indexByteArray struct sockaddr_in, sin_port} arr 0
        , address = #{indexByteArray struct sockaddr_in, sin_addr.s_addr} arr 0
        }
      else Nothing
    else Nothing

-- | This is unsafe, but it is needed for the wrappers of @recvmmsg@.
-- The index uses @sockaddr_in@s as elements, not bytes. The caller of this
-- function is responsible for bounds checks. Returns the actual (non-internet)
-- socket family on a failure to parse.
indexSocketAddressInternet :: Addr -> Int -> IO (Either CInt SocketAddressInternet)
indexSocketAddressInternet addr ix = do
  fam <- #{peek struct sockaddr_in, sin_family} ptr
  if fam == (#{const AF_INET} :: CUShort)
    then do
      port <- #{peek struct sockaddr_in, sin_port} ptr
      address <- #{peek struct sockaddr_in, sin_addr.s_addr} ptr
      pure (Right (SocketAddressInternet { port, address }))
    else pure (Left (cushortToCInt fam))
  where
  !(Addr offAddr) = PMA.plusAddr addr (ix * (#{size struct sockaddr_in}))
  ptr = Ptr offAddr

-- | Serialize a unix domain socket address so that it may be passed to @bind@.
--   This serialization is operating-system dependent. If the path provided by
--   the argument equals or exceeds the size of @sun_path@ (typically in the range 92
--   to 108 but varies by platform), the socket address will instead be given the
--   empty string as its path. This typically results in @bind@ returning an
--   error code.
encodeSocketAddressUnix :: SocketAddressUnix -> SocketAddress
encodeSocketAddressUnix (SocketAddressUnix !name) =
  SocketAddress $ runByteArrayST $ unboxByteArrayST $ do 
    -- On linux, sun_path always has exactly 108 bytes. It is a null-terminated
    -- string, so we initialize the byte array to zeroes to ensure this
    -- happens.
    let pathSize = 108 :: Int
    -- Again, we hard-code the size of sa_family_t as the size of
    -- an unsigned short.
    let familySize = FS.sizeOf (undefined :: CUShort)
    bs <- PM.newByteArray (pathSize + familySize)
    PM.setByteArray bs familySize pathSize (0 :: Word8)
    PM.writeByteArray bs 0 (#{const AF_UNIX} :: CUShort)
    let sz = PM.sizeofByteArray name
    when (sz < pathSize) $ do
      PM.copyByteArray bs familySize name 0 sz
    PM.unsafeFreezeByteArray bs

cushortToCInt :: CUShort -> CInt
cushortToCInt = fromIntegral

unboxByteArrayST :: ST s ByteArray -> State## s -> (## State## s, ByteArray## ##)
unboxByteArrayST (ST f) s = case f s of
  (## s', ByteArray b ##) -> (## s', b ##)

-- This is a specialization of runST that avoids a needless
-- data constructor allocation.
runByteArrayST :: (State## RealWorld -> (## State## RealWorld, ByteArray## ##)) -> ByteArray
runByteArrayST st_rep = case runRW## st_rep of (## _, a ##) -> ByteArray a

