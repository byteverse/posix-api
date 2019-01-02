#include <sys/socket.h>

-- | All of the data constructors provided by this module are unsafe.
--   Only use them if you really know what you are doing.
module Linux.Socket.Types
  ( dontWait
  ) where

import Posix.Socket (Flags(..))

-- | The @MSG_DONTWAIT@ receive flag or send flag.
dontWait :: Flags m
dontWait = Flags #{const MSG_DONTWAIT}

