{-# language DerivingStrategies #-}
{-# language DuplicateRecordFields #-}
{-# language GeneralizedNewtypeDeriving #-}

#include <sys/socket.h>

-- | All of the data constructors provided by this module are unsafe.
--   Only use them if you really know what you are doing.
module Posix.Socket.Types
  ( Domain(..)
  , Type(..)
  , Protocol(..)
  , SocketAddress(..)
  , SocketAddressInternet(..)
  , SocketAddressInternet6(..)
  , SocketAddressUnix(..)
  , SendFlags(..)
  , ReceiveFlags(..)
    -- * Socket Families
  , unix
  , unspecified
  , internet
    -- * Socket Types
  , stream
  , datagram
  , raw
  , sequencedPacket
    -- * Protocols
  , defaultProtocol
    -- * Receive Flags
  , peek
  , outOfBand
  , waitAll
  ) where

import Foreign.C.Types (CInt(..))
import Data.Primitive (ByteArray)
import Data.Bits (Bits,(.|.))
import Data.Word (Word16,Word32,Word64)

-- | A socket communications domain, sometimes referred to as a family. The spec
--   mandates @AF_UNIX@, @AF_UNSPEC@, and @AF_INET@.
newtype Domain = Domain CInt

-- | A socket type. The spec mandates @SOCK_STREAM@, @SOCK_DGRAM@,
--   and @SOCK_SEQPACKET@. Other types may be available on a per-platform
--   basis.
newtype Type = Type CInt

newtype Protocol = Protocol CInt

newtype SendFlags = SendFlags CInt
  deriving stock (Eq)
  deriving newtype (Bits)

instance Semigroup SendFlags where (<>) = (.|.)
instance Monoid SendFlags where mempty = SendFlags 0

newtype ReceiveFlags = ReceiveFlags CInt
  deriving stock (Eq)
  deriving newtype (Bits)

instance Semigroup ReceiveFlags where (<>) = (.|.)
instance Monoid ReceiveFlags where mempty = ReceiveFlags 0

-- | The @sockaddr@ data. This is an extensible tagged union, so this
--   library has chosen to represent it as byte array. It is up to
--   platform-specific libraries to inhabit this type with values.
--   The byte array representing the socket address must be pinned
--   since @bind@ uses a safe FFI call.
newtype SocketAddress = SocketAddress ByteArray

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

-- | The @MSG_PEEK@ receive flag.
peek :: ReceiveFlags
peek = ReceiveFlags #{const MSG_PEEK}

-- | The @MSG_OOB@ receive flag.
outOfBand :: ReceiveFlags
outOfBand = ReceiveFlags #{const MSG_OOB}

-- | The @MSG_WAITALL@ receive flag.
waitAll :: ReceiveFlags
waitAll = ReceiveFlags #{const MSG_WAITALL}

defaultProtocol :: Protocol
defaultProtocol = Protocol 0

