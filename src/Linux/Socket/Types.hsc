{-# language DataKinds #-}
{-# language DerivingStrategies #-}
{-# language DuplicateRecordFields #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language BinaryLiterals #-}
{-# language TypeApplications #-}

-- This is needed because hsc2hs does not currently handle ticked
-- promoted data constructors correctly.
{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

#define _GNU_SOURCE
#include <sys/socket.h>
#include <netinet/in.h>
#include <netinet/ip.h>
#include <netinet/udp.h>
#include <asm/byteorder.h>
#include "custom.h"

-- | All of the data constructors provided by this module are unsafe.
--   Only use them if you really know what you are doing.
module Linux.Socket.Types
  ( SocketFlags(..)
  , dontWait
  , truncate
  , controlTruncate
  , closeOnExec
  , nonblocking
  , headerInclude
    -- * Multiple Message Header
  , pokeMultipleMessageHeaderName
  , pokeMultipleMessageHeaderNameLength
  , pokeMultipleMessageHeaderIOVector
  , pokeMultipleMessageHeaderIOVectorLength
  , pokeMultipleMessageHeaderControl
  , pokeMultipleMessageHeaderControlLength
  , pokeMultipleMessageHeaderFlags
  , pokeMultipleMessageHeaderLength
  , peekMultipleMessageHeaderLength
  , peekMultipleMessageHeaderNameLength
  , sizeofMultipleMessageHeader
    -- * UDP Header
  , sizeofUdpHeader
  , pokeUdpHeaderSourcePort
  , pokeUdpHeaderDestinationPort
  , pokeUdpHeaderLength
  , pokeUdpHeaderChecksum
    -- * IPv4 Header
  , sizeofIpHeader
  , pokeIpHeaderVersionIhl
  , pokeIpHeaderTypeOfService
  , pokeIpHeaderTotalLength
  , pokeIpHeaderIdentifier
  , pokeIpHeaderFragmentOffset
  , pokeIpHeaderTimeToLive
  , pokeIpHeaderProtocol
  , pokeIpHeaderChecksum
  , pokeIpHeaderSourceAddress
  , pokeIpHeaderDestinationAddress
  ) where

import Prelude hiding (truncate)

import Data.Bits (Bits((.|.)))
import Data.Word (Word8,Word16,Word32)
import Data.Primitive.Addr (Addr(..),writeOffAddr)
import Foreign.C.Types (CInt(..),CSize,CUInt)
import Posix.Socket (MessageFlags(..),Message(Receive),OptionName(..))
import Foreign.Storable (peekByteOff,pokeByteOff)
import GHC.Ptr (Ptr(..))

newtype SocketFlags = SocketFlags CInt
  deriving stock (Eq)
  deriving newtype (Bits)

-- newtype Collection :: Type -> Type where
--   Collection :: !(Ptr a) -> Collection a
-- 
-- indexCollection :: Ptr a
--   => Collection a -> Int -> IO (Ptr a)
-- indexCollection (Collection p) n = advancePtr p n
-- data MessageHeader = MessageHeader
--   { name :: !Addr
--   , nameLength :: !CInt
--   , vector :: !(Ptr InputOutputVector)
--   , vectorLength :: !CSize
--   , control :: !(Ptr ControlMessageHeader)
--   , controlLength :: !CSize
--   , flags :: !(MessageFlags Receive)
--   }
-- 
-- data IOVector = IOVector
--   { base :: !Addr
--   , length :: !CSize
--   }
-- 
-- data ControlMessageHeader = ControlMessageHeader
--   { length :: !CInt
--   , level :: !CInt
--   , type_ :: !CInt
--   , data_ :: !Addr
--   }

instance Semigroup SocketFlags where (<>) = (.|.)
instance Monoid SocketFlags where mempty = SocketFlags 0

-- | The @MSG_DONTWAIT@ receive flag or send flag.
dontWait :: MessageFlags m
dontWait = MessageFlags #{const MSG_DONTWAIT}

-- | The @MSG_TRUNC@ receive flag.
truncate :: MessageFlags Receive
truncate = MessageFlags #{const MSG_TRUNC}

-- | The @MSG_CTRUNC@ receive flag.
controlTruncate :: MessageFlags Receive
controlTruncate = MessageFlags #{const MSG_CTRUNC}

-- | The @SOCK_CLOEXEC@ receive flag or send flag.
closeOnExec :: SocketFlags
closeOnExec = SocketFlags #{const SOCK_CLOEXEC}

-- | The @SOCK_NONBLOCK@ receive flag or send flag.
nonblocking :: SocketFlags
nonblocking = SocketFlags #{const SOCK_NONBLOCK}

-- | If enabled, the user supplies an IP header in front of the
-- user data.  Valid only for @SOCK_RAW@ sockets.
headerInclude :: OptionName
headerInclude = OptionName #{const IP_HDRINCL}

-- | The size of a @mmsghdr@ struct.
sizeofMultipleMessageHeader :: CInt
sizeofMultipleMessageHeader = #{size struct mmsghdr}

pokeMultipleMessageHeaderName :: Addr -> Addr -> IO ()
pokeMultipleMessageHeaderName (Addr p) (Addr x) = #{poke struct mmsghdr, msg_hdr.msg_name} (Ptr p) (Ptr x)

pokeMultipleMessageHeaderNameLength :: Addr -> CInt -> IO ()
pokeMultipleMessageHeaderNameLength (Addr p) = #{poke struct mmsghdr, msg_hdr.msg_namelen} (Ptr p)

pokeMultipleMessageHeaderIOVector :: Addr -> Addr -> IO ()
pokeMultipleMessageHeaderIOVector (Addr p) (Addr x) = #{poke struct mmsghdr, msg_hdr.msg_iov} (Ptr p) (Ptr x)

pokeMultipleMessageHeaderIOVectorLength :: Addr -> CSize -> IO ()
pokeMultipleMessageHeaderIOVectorLength (Addr p) = #{poke struct mmsghdr, msg_hdr.msg_iovlen} (Ptr p)

pokeMultipleMessageHeaderControl :: Addr -> Addr -> IO ()
pokeMultipleMessageHeaderControl (Addr p) (Addr x) = #{poke struct mmsghdr, msg_hdr.msg_control} (Ptr p) (Ptr x)

pokeMultipleMessageHeaderControlLength :: Addr -> CSize -> IO ()
pokeMultipleMessageHeaderControlLength (Addr p) = #{poke struct mmsghdr, msg_hdr.msg_controllen} (Ptr p)

pokeMultipleMessageHeaderFlags :: Addr -> MessageFlags Receive -> IO ()
pokeMultipleMessageHeaderFlags (Addr p) (MessageFlags i) = #{poke struct mmsghdr, msg_hdr.msg_flags} (Ptr p) i

pokeMultipleMessageHeaderLength :: Addr -> CUInt -> IO ()
pokeMultipleMessageHeaderLength (Addr p) i = #{poke struct mmsghdr, msg_len} (Ptr p) i

peekMultipleMessageHeaderNameLength :: Addr -> IO CInt
peekMultipleMessageHeaderNameLength (Addr p) = #{peek struct mmsghdr, msg_hdr.msg_namelen} (Ptr p)

peekMultipleMessageHeaderLength :: Addr -> IO CUInt
peekMultipleMessageHeaderLength (Addr p) = #{peek struct mmsghdr, msg_len} (Ptr p)

-- | The size of a @udphdr@ struct.
sizeofUdpHeader :: CInt
sizeofUdpHeader = #{size struct udphdr}

pokeUdpHeaderSourcePort :: Addr -> Word16 -> IO ()
pokeUdpHeaderSourcePort (Addr p) = #{poke struct udphdr, source} (Ptr p)

pokeUdpHeaderDestinationPort :: Addr -> Word16 -> IO ()
pokeUdpHeaderDestinationPort (Addr p) = #{poke struct udphdr, dest} (Ptr p)

pokeUdpHeaderLength :: Addr -> Word16 -> IO ()
pokeUdpHeaderLength (Addr p) = #{poke struct udphdr, len} (Ptr p)

pokeUdpHeaderChecksum :: Addr -> Word16 -> IO ()
pokeUdpHeaderChecksum (Addr p) = #{poke struct udphdr, check} (Ptr p)

-- | The size of an @iphdr@ struct.
sizeofIpHeader :: CInt
sizeofIpHeader = #{size struct iphdr}

-- | This poke function requires the user to pack the version and the
-- internet header length (IHL), each 4 bits, into a single 8-bit word.
-- The version should be in the most significant bits. This function
-- will marshal the value appropriately depending on the platform's
-- bit-endianness.
pokeIpHeaderVersionIhl :: Addr -> Word8 -> IO ()
-- TODO: Verify if this is correct. Also, something bad is going
-- on here. Fix this.
#if defined(__LITTLE_ENDIAN_BITFIELD)
pokeIpHeaderVersionIhl p _ = writeOffAddr p 0 (0b01000101 :: Word8)
#elif defined (__BIG_ENDIAN_BITFIELD)
pokeIpHeaderVersionIhl p w = PM.writeOffAddr p 0 w
#else
ERROR_BITFIELD_ENDIANNESS_NOT_SET
#endif

pokeIpHeaderTypeOfService :: Addr -> Word8 -> IO ()
pokeIpHeaderTypeOfService (Addr p) = #{poke struct iphdr, tos} (Ptr p)

pokeIpHeaderTotalLength :: Addr -> Word16 -> IO ()
pokeIpHeaderTotalLength (Addr p) = #{poke struct iphdr, tot_len} (Ptr p)

pokeIpHeaderIdentifier :: Addr -> Word16 -> IO ()
pokeIpHeaderIdentifier (Addr p) = #{poke struct iphdr, id} (Ptr p)

pokeIpHeaderFragmentOffset :: Addr -> Word16 -> IO ()
pokeIpHeaderFragmentOffset (Addr p) = #{poke struct iphdr, frag_off} (Ptr p)

pokeIpHeaderTimeToLive :: Addr -> Word8 -> IO ()
pokeIpHeaderTimeToLive (Addr p) = #{poke struct iphdr, ttl} (Ptr p)

pokeIpHeaderProtocol :: Addr -> Word8 -> IO ()
pokeIpHeaderProtocol (Addr p) = #{poke struct iphdr, protocol} (Ptr p)

pokeIpHeaderChecksum :: Addr -> Word16 -> IO ()
pokeIpHeaderChecksum (Addr p) = #{poke struct iphdr, check} (Ptr p)

pokeIpHeaderSourceAddress :: Addr -> Word32 -> IO ()
pokeIpHeaderSourceAddress (Addr p) = #{poke struct iphdr, saddr} (Ptr p)

pokeIpHeaderDestinationAddress :: Addr -> Word32 -> IO ()
pokeIpHeaderDestinationAddress (Addr p) = #{poke struct iphdr, daddr} (Ptr p)
