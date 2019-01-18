{-# language DerivingStrategies #-}
{-# language GeneralizedNewtypeDeriving #-}

#include <sys/socket.h>

-- | All of the data constructors provided by this module are unsafe.
--   Only use them if you really know what you are doing.
module Linux.Socket.Types
  ( SocketFlags(..)
  , dontWait
  , truncate
  , closeOnExec
  , nonblocking
  ) where

import Prelude hiding (truncate)

import Data.Bits (Bits((.|.)))
import Foreign.C.Types (CInt)
import Posix.Socket (MessageFlags(..))

newtype SocketFlags = SocketFlags CInt
  deriving stock (Eq)
  deriving newtype (Bits)

instance Semigroup SocketFlags where (<>) = (.|.)
instance Monoid SocketFlags where mempty = SocketFlags 0

-- | The @MSG_DONTWAIT@ receive flag or send flag.
dontWait :: MessageFlags m
dontWait = MessageFlags #{const MSG_DONTWAIT}

-- | The @MSG_TRUNC@ receive flag.
truncate :: MessageFlags m
truncate = MessageFlags #{const MSG_DONTWAIT}

-- | The @SOCK_CLOEXEC@ receive flag or send flag.
closeOnExec :: SocketFlags
closeOnExec = SocketFlags #{const SOCK_CLOEXEC}

-- | The @SOCK_NONBLOCK@ receive flag or send flag.
nonblocking :: SocketFlags
nonblocking = SocketFlags #{const SOCK_NONBLOCK}

