{-# language DataKinds #-}
{-# language DerivingStrategies #-}
{-# language DuplicateRecordFields #-}
{-# language GADTSyntax #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language KindSignatures #-}

-- This is needed because hsc2hs does not currently handle ticked
-- promoted data constructors correctly.
{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

#include <sys/socket.h>
#include <netinet/in.h>

-- | All of the data constructors provided by this module are unsafe.
--   Only use them if you really know what you are doing.
module Posix.Socket.Types
  ( Domain(..)
  , Type(..)
  , Protocol(..)
  , Level(..)
  , OptionName(..)
  , OptionValue(..)
  , SocketAddress(..)
  , SocketAddressInternet(..)
  , SocketAddressInternet6(..)
  , SocketAddressUnix(..)
  , MessageFlags(..)
  , Message(..)
  , ShutdownType(..)
    -- * Socket Families
  , unix
  , unspecified
  , internet
  , internet6
    -- * Socket Types
  , stream
  , datagram
  , raw
  , sequencedPacket
    -- * Protocols
  , defaultProtocol
  , rawProtocol
  , icmp
  , tcp
  , udp
  , ip
  , ipv6
    -- * Receive Flags
  , peek
  , outOfBand
  , waitAll
    -- * Shutdown Types
  , read
  , write
  , readWrite
    -- * Socket Levels
  , levelSocket
    -- * Option Names
  , optionError
    -- * Message Header
    -- ** Peek
  , peekMessageHeaderName
  , peekMessageHeaderNameLength
  , peekMessageHeaderIOVector
  , peekMessageHeaderIOVectorLength
  , peekMessageHeaderControl
  , peekMessageHeaderControlLength
  , peekMessageHeaderFlags
  , peekControlMessageHeaderLevel
  , peekControlMessageHeaderLength
  , peekControlMessageHeaderType
    -- ** Poke
  , pokeMessageHeaderName
  , pokeMessageHeaderNameLength
  , pokeMessageHeaderIOVector
  , pokeMessageHeaderIOVectorLength
  , pokeMessageHeaderControl
  , pokeMessageHeaderControlLength
  , pokeMessageHeaderFlags
    -- ** Metadata
  , sizeofMessageHeader
    -- * IO Vector
    -- ** Peek
  , peekIOVectorBase
  , peekIOVectorLength
    -- ** Poke
  , pokeIOVectorBase
  , pokeIOVectorLength
    -- ** Metadata
  , sizeofIOVector
  ) where

import Prelude hiding (read)

import Data.Bits (Bits,(.|.))
import Data.Primitive (ByteArray,Addr(..))
import Data.Word (Word16,Word32,Word64)
import Foreign.C.Types (CInt(..),CSize)
import Foreign.Storable (peekByteOff,pokeByteOff)
import GHC.Ptr (Ptr(..))

import qualified Data.Kind

-- | A socket communications domain, sometimes referred to as a family. The spec
--   mandates @AF_UNIX@, @AF_UNSPEC@, and @AF_INET@.
newtype Domain = Domain CInt

-- | A socket type. The spec mandates @SOCK_STREAM@, @SOCK_DGRAM@,
--   and @SOCK_SEQPACKET@. Other types may be available on a per-platform
--   basis.
newtype Type = Type CInt

newtype Protocol = Protocol CInt

newtype Level = Level CInt

newtype OptionName = OptionName CInt

-- | Which end of the socket to shutdown.
newtype ShutdownType = ShutdownType CInt

-- | The direction of a message. The data constructor are only used
--   at the type level as phantom arguments.
data Message = Send | Receive

-- | Receive flags are given by @MessageFlags Receive@ and send flags
--   are given by @MessageFlags Send@. This is done because there are
--   several flags that are applicable in either a receiving
--   context or a sending context.
newtype MessageFlags :: Message -> Data.Kind.Type where
  MessageFlags :: CInt -> MessageFlags m
  deriving stock (Eq)
  deriving newtype (Bits)

instance Semigroup (MessageFlags m) where (<>) = (.|.)
instance Monoid (MessageFlags m) where mempty = MessageFlags 0

-- | The @sockaddr@ data. This is an extensible tagged union, so this library
--   has chosen to represent it as byte array. It is up to platform-specific
--   libraries to inhabit this type with values. The byte array backing this
--   may be unpinned or pinned.
newtype SocketAddress = SocketAddress ByteArray
  deriving newtype (Eq,Show)

-- | The @option_value@ data.
newtype OptionValue = OptionValue ByteArray

-- | An address for an Internet socket over IPv4. The
--   <http://pubs.opengroup.org/onlinepubs/000095399/basedefs/netinet/in.h.html POSIX specification>
--   mandates three fields:
--
--   > sa_family_t     sin_family   AF_INET
--   > in_port_t       sin_port     Port number
--   > struct in_addr  sin_addr     IP address
--
--   This type omits the first field since is a constant that
--   is only relevant for serialization purposes. The spec also
--   mandates that @sin_port@ and @sin_addr@ be in network
--   byte order, so keep in mind that these values are not
--   immidiately useable.
data SocketAddressInternet = SocketAddressInternet
  { port :: !Word16
  , address :: !Word32
  }

-- Revisit this. We really need a standard Word128 type somewhere.
data SocketAddressInternet6 = SocketAddressInternet6
  { port :: !Word16
  , flowInfo :: !Word32
  , addressA :: !Word64
  , addressB :: !Word64
  , scopeId :: !Word32
  }

-- | An address for a UNIX domain socket. The
--   <http://pubs.opengroup.org/onlinepubs/009604499/basedefs/sys/un.h.html POSIX specification>
--   mandates two fields:
--
--   > sa_family_t  sun_family  Address family. 
--   > char         sun_path[]  Socket pathname. 
--
--   However, the first field is omitted since it is always @AF_UNIX@.
--   It is adding during serialization. Although @sun_path@ is a
--   null-terminated string, @SocketAddressUnix@ should not have
--   a trailing null byte. The conversion function @encodeSocketAddressUnix@
--   adds the null terminator. The size of path should not equal
--   or exceed the platform-dependent size of @sun_path@.
newtype SocketAddressUnix = SocketAddressUnix
  { path :: ByteArray
  }

-- | The @SOCK_STREAM@ socket type.
stream :: Type
stream = Type #{const SOCK_STREAM}

-- | The @SOCK_DGRAM@ socket type.
datagram :: Type
datagram = Type #{const SOCK_DGRAM}

-- | The @SOCK_RAW@ socket type. POSIX declares raw sockets optional.
--   However, they are included here for convenience. Please open an
--   issue if this prevents this library from compiling on a
--   POSIX-compliant operating system that anyone uses for haskell
--   development. Keep in mind that even though raw sockets may exist
--   on all POSIX-compliant operating systems, they may differ in
--   their behavior.
raw :: Type
raw = Type #{const SOCK_RAW}

-- | The @SOCK_SEQPACKET@ socket type.
sequencedPacket :: Type
sequencedPacket = Type #{const SOCK_SEQPACKET}

-- | The @AF_UNIX@ communications domain.
unix :: Domain
unix = Domain #{const AF_UNIX}

-- | The @AF_UNSPEC@ communications domain.
unspecified :: Domain
unspecified = Domain #{const AF_UNSPEC}

-- | The @AF_INET@ communications domain.
internet :: Domain
internet = Domain #{const AF_INET}

-- | The @AF_INET6@ communications domain. POSIX declares raw sockets
--   optional. However, they are included here for convenience. Please
--   open an issue if this prevents this library from compiling on a
--   POSIX-compliant operating system that anyone uses for haskell
--   development.
internet6 :: Domain
internet6 = Domain #{const AF_INET6}

-- | The @MSG_OOB@ receive flag or send flag.
outOfBand :: MessageFlags m
outOfBand = MessageFlags #{const MSG_OOB}

-- | The @MSG_PEEK@ receive flag.
peek :: MessageFlags Receive
peek = MessageFlags #{const MSG_PEEK}

-- | The @MSG_WAITALL@ receive flag.
waitAll :: MessageFlags Receive
waitAll = MessageFlags #{const MSG_WAITALL}

-- | The default protocol for a socket type.
defaultProtocol :: Protocol
defaultProtocol = Protocol 0

-- | The @IPPROTO_RAW@ protocol.
rawProtocol :: Protocol
rawProtocol = Protocol #{const IPPROTO_RAW}

-- | The @IPPROTO_ICMP@ protocol.
icmp :: Protocol
icmp = Protocol #{const IPPROTO_ICMP}

-- | The @IPPROTO_TCP@ protocol.
tcp :: Protocol
tcp = Protocol #{const IPPROTO_TCP}

-- | The @IPPROTO_UDP@ protocol.
udp :: Protocol
udp = Protocol #{const IPPROTO_UDP}

-- | The @IPPROTO_IP@ protocol.
ip :: Protocol
ip = Protocol #{const IPPROTO_IP}

-- | The @IPPROTO_IPV6@ protocol.
ipv6 :: Protocol
ipv6 = Protocol #{const IPPROTO_IPV6}

-- | Disable further receive operations (e.g. @SHUT_RD@)
read :: ShutdownType
read = ShutdownType #{const SHUT_RD}

-- | Disable further send operations (e.g. @SHUT_WR@)
write :: ShutdownType
write = ShutdownType #{const SHUT_WR}

-- | Disable further send operations (e.g. @SHUT_RDWR@)
readWrite :: ShutdownType
readWrite = ShutdownType #{const SHUT_RDWR}

-- | Socket error status (e.g. @SOL_SOCKET@)
levelSocket :: Level
levelSocket = Level #{const SOL_SOCKET}

-- | Socket error status (e.g. @SO_ERROR@)
optionError :: OptionName
optionError = OptionName #{const SO_ERROR}

peekControlMessageHeaderLength :: Addr -> IO CInt
peekControlMessageHeaderLength (Addr p) = #{peek struct cmsghdr, cmsg_len} (Ptr p)

peekControlMessageHeaderLevel :: Addr -> IO Level
peekControlMessageHeaderLevel (Addr p) = do
  i <- #{peek struct cmsghdr, cmsg_level} (Ptr p)
  pure (Level i)

peekControlMessageHeaderType :: Addr -> IO Type
peekControlMessageHeaderType (Addr p) = do
  i <- #{peek struct cmsghdr, cmsg_type} (Ptr p)
  pure (Type i)

-- Think about reintroducing this function when it becomes necessary.
-- advanceControlMessageHeaderData :: Addr -> Addr
-- advanceControlMessageHeaderData p =
--   PM.plusAddr p (#{size struct cmsghdr})

peekIOVectorBase :: Addr -> IO Addr
peekIOVectorBase (Addr p) = do
  Ptr x <- #{peek struct iovec, iov_base} (Ptr p)
  pure (Addr x)

peekIOVectorLength :: Addr -> IO CSize
peekIOVectorLength (Addr p) = #{peek struct iovec, iov_len} (Ptr p)

-- | The size of a serialized @msghdr@.
sizeofMessageHeader :: CInt
sizeofMessageHeader = #{size struct msghdr}

-- | The size of a serialized @iovec@.
sizeofIOVector :: CInt
sizeofIOVector = #{size struct iovec}

peekMessageHeaderName :: Addr -> IO Addr
peekMessageHeaderName (Addr p) = do
  Ptr x <- #{peek struct msghdr, msg_name} (Ptr p)
  pure (Addr x)

pokeMessageHeaderName :: Addr -> Addr -> IO ()
pokeMessageHeaderName (Addr p) (Addr x) = #{poke struct msghdr, msg_name} (Ptr p) (Ptr x)

pokeMessageHeaderNameLength :: Addr -> CInt -> IO ()
pokeMessageHeaderNameLength (Addr p) = #{poke struct msghdr, msg_namelen} (Ptr p)

pokeMessageHeaderIOVector :: Addr -> Addr -> IO ()
pokeMessageHeaderIOVector (Addr p) (Addr x) = #{poke struct msghdr, msg_iov} (Ptr p) (Ptr x)

pokeMessageHeaderIOVectorLength :: Addr -> CSize -> IO ()
pokeMessageHeaderIOVectorLength (Addr p) = #{poke struct msghdr, msg_iovlen} (Ptr p)

pokeMessageHeaderControl :: Addr -> Addr -> IO ()
pokeMessageHeaderControl (Addr p) (Addr x) = #{poke struct msghdr, msg_control} (Ptr p) (Ptr x)

pokeMessageHeaderControlLength :: Addr -> CSize -> IO ()
pokeMessageHeaderControlLength (Addr p) = #{poke struct msghdr, msg_controllen} (Ptr p)

pokeMessageHeaderFlags :: Addr -> MessageFlags Receive -> IO ()
pokeMessageHeaderFlags (Addr p) (MessageFlags i) = #{poke struct msghdr, msg_flags} (Ptr p) i

peekMessageHeaderNameLength :: Addr -> IO CInt
peekMessageHeaderNameLength (Addr p) = #{peek struct msghdr, msg_namelen} (Ptr p)

peekMessageHeaderIOVector :: Addr -> IO Addr
peekMessageHeaderIOVector (Addr p) = do
  Ptr r <- #{peek struct msghdr, msg_iov} (Ptr p)
  pure (Addr r)

peekMessageHeaderIOVectorLength :: Addr -> IO CSize
peekMessageHeaderIOVectorLength (Addr p) = #{peek struct msghdr, msg_iovlen} (Ptr p)

peekMessageHeaderControl :: Addr -> IO Addr
peekMessageHeaderControl (Addr p) = do
  Ptr r <- #{peek struct msghdr, msg_control} (Ptr p)
  pure (Addr r)

pokeIOVectorBase :: Addr -> Addr -> IO ()
pokeIOVectorBase (Addr p) (Addr x) = #{poke struct iovec, iov_base} (Ptr p) (Ptr x)

pokeIOVectorLength :: Addr -> CSize -> IO ()
pokeIOVectorLength (Addr p) = #{poke struct iovec, iov_len} (Ptr p)


peekMessageHeaderControlLength :: Addr -> IO CSize
peekMessageHeaderControlLength (Addr p) = #{peek struct msghdr, msg_controllen} (Ptr p)

peekMessageHeaderFlags :: Addr -> IO (MessageFlags Receive)
peekMessageHeaderFlags (Addr p) = do
  i <- #{peek struct msghdr, msg_flags} (Ptr p)
  pure (MessageFlags i)

