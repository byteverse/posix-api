{-# language DerivingStrategies #-}
{-# language GeneralizedNewtypeDeriving #-}

#include <sys/socket.h>

-- | All of the data constructors provided by this module are unsafe.
--   Only use them if you really know what you are doing.
module Posix.Socket.Types
  ( Family(..)
  , Type(..)
  , Protocol(..)
  , SocketAddress(..)
  , SendFlags(..)
  , ReceiveFlags(..)
    -- * Socket Families
  , unix
  , unspecified
  , internet
    -- * Socket Types
  , stream
  , datagram
  , sequencedPacket
    -- * Receive Flags
  , peek
  , outOfBand
  , waitAll
  ) where

import Foreign.C.Types (CInt(..))
import Data.Primitive (ByteArray)
import Data.Bits (Bits)

-- | A socket family, also referred to as a socket domain. The spec mandates
--   @AF_UNIX@, @AF_UNSPEC@, @AF_INET@.
newtype Family = Family CInt

-- | A socket type. The spec mandates @SOCK_STREAM@, @SOCK_DGRAM@,
--   and @SOCK_SEQPACKET@. Other types may be available on a per-platform
--   basis.
newtype Type = Type CInt

newtype Protocol = Protocol CInt

newtype SendFlags = SendFlags CInt
  deriving stock (Eq)
  deriving newtype (Bits)

newtype ReceiveFlags = ReceiveFlags CInt
  deriving stock (Eq)
  deriving newtype (Bits)

-- | The @sockaddr@ data. This is an extensible tagged union, so this
--   library has chosen to represent it as byte array. It is up to
--   platform-specific libraries to inhabit this type with values.
--   The byte array representing the socket address must be pinned
--   since @bind@ uses a safe FFI call.
newtype SocketAddress = SocketAddress ByteArray

-- | The @SOCK_STREAM@ socket type.
stream :: Type
stream = Type #{const SOCK_STREAM}

-- | The @SOCK_DGRAM@ socket type.
datagram :: Type
datagram = Type #{const SOCK_DGRAM}

-- | The @SOCK_SEQPACKET@ socket type.
sequencedPacket :: Type
sequencedPacket = Type #{const SOCK_SEQPACKET}

-- | The @AF_UNIX@ socket family.
unix :: Family
unix = Family #{const AF_UNIX}

-- | The @AF_UNSPEC@ socket family.
unspecified :: Family
unspecified = Family #{const AF_UNSPEC}

-- | The @AF_INET@ socket family.
internet :: Family
internet = Family #{const AF_INET}

-- | The @MSG_PEEK@ receive flag.
peek :: ReceiveFlags
peek = ReceiveFlags #{const MSG_PEEK}

-- | The @MSG_OOB@ receive flag.
outOfBand :: ReceiveFlags
outOfBand = ReceiveFlags #{const MSG_OOB}

-- | The @MSG_WAITALL@ receive flag.
waitAll :: ReceiveFlags
waitAll = ReceiveFlags #{const MSG_WAITALL}

