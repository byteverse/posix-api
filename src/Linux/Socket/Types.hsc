{-# language DataKinds #-}
{-# language DerivingStrategies #-}
{-# language DuplicateRecordFields #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language TypeApplications #-}

-- This is needed because hsc2hs does not currently handle ticked
-- promoted data constructors correctly.
{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

#define _GNU_SOURCE
#include <sys/socket.h>
#include <netinet/in.h>
#include <netinet/ip.h>

-- | All of the data constructors provided by this module are unsafe.
--   Only use them if you really know what you are doing.
module Linux.Socket.Types
  ( SocketFlags(..)
  , dontWait
  , truncate
  , controlTruncate
  , closeOnExec
  , nonblocking
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
  ) where

import Prelude hiding (truncate)

import Data.Bits (Bits((.|.)))
import Data.Primitive (Addr(..))
import Foreign.C.Types (CInt(..),CSize,CUInt)
import Posix.Socket (MessageFlags(..),Message(Receive))
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

-- | The size of a serialized @mmsghdr@.
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

