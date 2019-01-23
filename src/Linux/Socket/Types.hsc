{-# language DataKinds #-}
{-# language DerivingStrategies #-}
{-# language DuplicateRecordFields #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language TypeApplications #-}

-- This is needed because hsc2hs does not currently handle ticked
-- promoted data constructors correctly.
{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

#include <sys/socket.h>

-- | All of the data constructors provided by this module are unsafe.
--   Only use them if you really know what you are doing.
module Linux.Socket.Types
  ( SocketFlags(..)
  , dontWait
  , truncate
  , controlTruncate
  , closeOnExec
  , nonblocking
  ) where

import Prelude hiding (truncate)

import Data.Bits (Bits((.|.)))
import Foreign.C.Types (CInt)
import Posix.Socket (MessageFlags(..),Message(Receive))

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
truncate = MessageFlags #{const MSG_DONTWAIT}

-- | The @MSG_CTRUNC@ receive flag.
controlTruncate :: MessageFlags Receive
controlTruncate = MessageFlags #{const MSG_CTRUNC}

-- | The @SOCK_CLOEXEC@ receive flag or send flag.
closeOnExec :: SocketFlags
closeOnExec = SocketFlags #{const SOCK_CLOEXEC}

-- | The @SOCK_NONBLOCK@ receive flag or send flag.
nonblocking :: SocketFlags
nonblocking = SocketFlags #{const SOCK_NONBLOCK}

