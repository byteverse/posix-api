{-# language BangPatterns #-}
{-# language BinaryLiterals #-}
{-# language DataKinds #-}
{-# language DerivingStrategies #-}
{-# language DuplicateRecordFields #-}
{-# language GADTSyntax #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language KindSignatures #-}
{-# language MagicHash #-}
{-# language NamedFieldPuns #-}
{-# language PolyKinds #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}
{-# language TypeInType #-}
{-# language UnboxedTuples #-}

-- This is needed because hsc2hs does not currently handle ticked
-- promoted data constructors correctly.
{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

#define _GNU_SOURCE
#include <sys/epoll.h>
#include "custom.h"

-- | All of the data constructors provided by this module are unsafe.
--   Only use them if you really know what you are doing.
module Linux.Epoll.Types
  ( EpollFlags(..)
  , ControlOperation(..)
  , Exchange(..)
  , Events(..)
  , Event(..)
  , PrimEpollData(..)
    -- * Flags
  , closeOnExec
  , add
  , modify
  , delete
    -- * Events
  , input
  , output
  , priority
  , hangup
  , readHangup
  , error
  , edgeTriggered
    -- * Events Combinators
  , containsAnyEvents
  , containsAllEvents
    -- * Marshalling
  , sizeofEvent
  , peekEventEvents
  , peekEventDataFd
  , peekEventDataPtr
  , peekEventDataU32
  , peekEventDataU64
  , pokeEventDataU64
  -- , readEventDataU64
  -- , writeEventDataU64
  ) where

import Prelude hiding (truncate,error)

import Data.Bits (Bits,(.&.),(.|.),unsafeShiftL,unsafeShiftR)
import Data.Kind (Type)
import Data.Primitive.Addr (Addr(..))
import Data.Primitive (Prim)
import Data.Primitive (indexByteArray##,writeByteArray##,readByteArray##)
import Data.Primitive (indexOffAddr##,readOffAddr##,writeOffAddr##)
import Data.Word (Word32,Word64)
import Foreign.C.Types (CInt(..))
import Foreign.Storable (Storable,peekByteOff,pokeByteOff)
import GHC.Exts (Int(I##),(+##),(*##))
import GHC.Exts (State##,Int##,Addr##,MutableByteArray##,ByteArray##)
import GHC.Ptr (Ptr(..))
import Posix.Poll (Exchange(..))
import System.Posix.Types (Fd(..))

import qualified Data.Primitive as PM

newtype ControlOperation = ControlOperation CInt
  deriving stock (Eq)

newtype EpollFlags = EpollFlags CInt
  deriving stock (Eq)
  deriving newtype (Bits)

instance Semigroup EpollFlags where (<>) = (.|.)
instance Monoid EpollFlags where mempty = EpollFlags 0

newtype Events :: Exchange -> Type where
  Events :: Word32 -> Events e
  deriving stock (Eq)
  deriving newtype (Bits,Storable,Prim)

instance Semigroup (Events e) where (<>) = (.|.)
instance Monoid (Events e) where mempty = Events 0

-- | A data type corresponding to @struct epoll_event@. Linux
-- defines this as:
--
-- > typedef union epoll_data {
-- >     void    *ptr;
-- >     int      fd;
-- >     uint32_t u32;
-- >     uint64_t u64;
-- > } epoll_data_t;
-- >
-- > struct epoll_event {
-- >     uint32_t     events; /* Epoll events */
-- >     epoll_data_t data;   /* User data variable */
-- > };
--
-- It is a little difficult to capture what this type conveys, but
-- we make an attempt. The second argument to the @Event@ type
-- constructor is either @Addr@, @Fd@, @Word32@, or @Word64@. This
-- corresponds to the four possibilities in the @epoll_data@ union
-- type. As long as the user monomorphizes this type when using
-- it, there should not be any performance penalty for the
-- flexibility afforded by this approach.
data Event :: Exchange -> Type -> Type where
  Event ::
    { events :: !(Events e)
      -- ^ Epoll events
    , payload :: !a
      -- ^ User data variable, named @data@ in @struct epoll_event@.
    } -> Event e a

class PrimEpollData a where
  indexByteArrayEpoll :: ByteArray## -> Int## -> Event e a
  readByteArrayEpoll :: MutableByteArray## s -> Int## -> State## s -> (## State## s, Event e a ##)
  writeByteArrayEpoll :: MutableByteArray## s -> Int## -> Event e a -> State## s -> State## s
  indexOffAddrEpoll :: Addr## -> Int## -> Event e a
  readOffAddrEpoll :: Addr## -> Int## -> State## s -> (## State## s, Event e a ##)
  writeOffAddrEpoll :: Addr## -> Int## -> Event e a -> State## s -> State## s

instance PrimEpollData a => Prim (Event e a) where
  {-# inline sizeOf# #-}
  {-# inline alignment# #-}
  {-# inline indexByteArray# #-}
  {-# inline readByteArray# #-}
  {-# inline writeByteArray# #-}
  {-# inline setByteArray# #-}
  {-# inline indexOffAddr# #-}
  {-# inline readOffAddr# #-}
  {-# inline writeOffAddr# #-}
  {-# inline setOffAddr# #-}
  sizeOf## _ = unI #{size struct epoll_event}
  alignment## _ = PM.alignment## (undefined :: Word32)
  indexByteArray## = indexByteArrayEpoll
  readByteArray## = readByteArrayEpoll
  writeByteArray## = writeByteArrayEpoll
  setByteArray## = PM.defaultSetByteArray##
  indexOffAddr## = indexOffAddrEpoll
  readOffAddr## = readOffAddrEpoll
  writeOffAddr## = writeOffAddrEpoll
  setOffAddr## = PM.defaultSetOffAddr##

instance PrimEpollData Fd where
  {-# inline indexByteArrayEpoll #-}
  {-# inline readByteArrayEpoll #-}
  {-# inline writeByteArrayEpoll #-}
  {-# inline indexOffAddrEpoll #-}
  {-# inline readOffAddrEpoll #-}
  {-# inline writeOffAddrEpoll #-}
  indexByteArrayEpoll arr i = Event
    { events = #{indexByteArrayHash struct epoll_event, events} arr i
    , payload = #{indexByteArrayHash struct epoll_event, data.fd} arr i
    }
  writeByteArrayEpoll arr i Event{events,payload} s0 =
    case #{writeByteArrayHash struct epoll_event, events} arr i events s0 of
      s1 -> #{writeByteArrayHash struct epoll_event, data.fd} arr i payload s1
  readByteArrayEpoll arr i s0 =
    case #{readByteArrayHash struct epoll_event, events} arr i s0 of
      (## s1, events ##) -> case #{readByteArrayHash struct epoll_event, data.fd} arr i s1 of
        (## s2, payload ##) -> (## s2, Event{events,payload} ##)
  indexOffAddrEpoll arr i = Event
    { events = #{indexOffAddrHash struct epoll_event, events} arr i
    , payload = #{indexOffAddrHash struct epoll_event, data.fd} arr i
    }
  writeOffAddrEpoll arr i Event{events,payload} s0 =
    case #{writeOffAddrHash struct epoll_event, events} arr i events s0 of
      s1 -> #{writeOffAddrHash struct epoll_event, data.fd} arr i payload s1
  readOffAddrEpoll arr i s0 =
    case #{readOffAddrHash struct epoll_event, events} arr i s0 of
      (## s1, events ##) -> case #{readOffAddrHash struct epoll_event, data.fd} arr i s1 of
        (## s2, payload ##) -> (## s2, Event{events,payload} ##)

-- | Since @epoll_event@ includes an unaligned 64-bit word, it is
-- difficult to use @hsc2hs@ to generate the marshalling code. Consequently,
-- the offsets of @events@ and @data@ are currently hardcoded. Open an
-- issue in this causes a problem on your platform.
instance PrimEpollData Word64 where
  {-# inline indexByteArrayEpoll #-}
  {-# inline readByteArrayEpoll #-}
  {-# inline writeByteArrayEpoll #-}
  {-# inline indexOffAddrEpoll #-}
  {-# inline readOffAddrEpoll #-}
  {-# inline writeOffAddrEpoll #-}
  indexByteArrayEpoll arr i = Event
    { events = PM.indexByteArray## arr (i *## 3##)
    , payload = composePayload
        (PM.indexByteArray## arr ((i *## 3##) +# 1##))
        (PM.indexByteArray## arr ((i *## 3##) +# 2##))
    }
  writeByteArrayEpoll arr i Event{events,payload} s0 = case PM.writeByteArray## arr (i *## 3##) events s0 of
    s1 -> case PM.writeByteArray## arr ((i *## 3##) +## 1##) pa s1 of
      s2 -> PM.writeByteArray## arr ((i *## 3##) +## 2##) pb s2
    where
    !(pa,pb) = decomposePayload payload
  readByteArrayEpoll arr i s0 = case PM.readByteArray## arr (i *## 3##) s0 of
    (## s1, events ##) -> case PM.readByteArray## arr ((i *## 3##) +## 1##) s1 of
      (## s2, pa ##) -> case PM.readByteArray## arr ((i *## 3##) +## 2##) s2 of
        (## s3, pb ##) -> let payload = composePayload pa pb in
          (## s3, Event{events,payload} ##)
  indexOffAddrEpoll arr i = Event
    { events = PM.indexOffAddr## arr (i *## 3##)
    , payload = composePayload
        (PM.indexOffAddr## arr ((i *## 3##) +## 1##))
        (PM.indexOffAddr## arr ((i *## 3##) +## 2##))
    }
  writeOffAddrEpoll arr i Event{events,payload} s0 = case PM.writeOffAddr## arr (i *## 3##) events s0 of
    s1 -> case PM.writeOffAddr## arr ((i *## 3##) +## 1##) pa s1 of
      s2 -> PM.writeOffAddr## arr ((i *## 3##) +## 2##) pb s2
    where
    !(pa,pb) = decomposePayload payload
  readOffAddrEpoll arr i s0 = case PM.readOffAddr## arr (i *## 3##) s0 of
    (## s1, events ##) -> case PM.readOffAddr## arr ((i *## 3##) +## 1##) s1 of
      (## s2, pa ##) -> case PM.readOffAddr## arr ((i *## 3##) +## 2##) s2 of
        (## s3, pb ##) -> let payload = composePayload pa pb in
          (## s3, Event{events,payload} ##)

-- | The @EPOLL_CTL_ADD@ control operation.
add :: ControlOperation
add = ControlOperation #{const EPOLL_CTL_ADD}

-- | The @EPOLL_CTL_MOD@ control operation.
modify :: ControlOperation
modify = ControlOperation #{const EPOLL_CTL_MOD}

-- | The @EPOLL_CTL_DEL@ control operation.
delete :: ControlOperation
delete = ControlOperation #{const EPOLL_CTL_DEL}

-- | The @EPOLL_CLOEXEC@ flag.
closeOnExec :: EpollFlags
closeOnExec = EpollFlags #{const EPOLL_CLOEXEC}

-- | The @EPOLLIN@ event. Can appear in a request or a response.
input :: Events e
input = Events #{const EPOLLIN}

-- | The @EPOLLOUT@ event. Can appear in a request or a response.
output :: Events e
output = Events #{const EPOLLOUT}

-- | The @EPOLLPRI@ event. Can appear in a request or a response.
priority :: Events e
priority = Events #{const EPOLLPRI}

-- | The @EPOLLERR@ event. The
-- <http://man7.org/linux/man-pages/man2/epoll_ctl.2.html epoll_ctl documentation> says
-- "@epoll_wait@ will always wait for this event; it is not necessary to set it in @events@".
-- Consequently, in this library, it has been marked as only appearing in @Response@ positions.
error :: Events Response
error = Events #{const EPOLLERR}

-- | The @EPOLLHUP@ event. The
-- <http://man7.org/linux/man-pages/man2/epoll_ctl.2.html epoll_ctl documentation> says
-- "@epoll_wait@ will always wait for this event; it is not necessary to set it in @events@".
-- Consequently, in this library, it has been marked as only appearing in @Response@ positions.
hangup :: Events Response
hangup = Events #{const EPOLLHUP}

-- | The @EPOLLRDHUP@ event. Can appear in a request or a response.
readHangup :: Events e
readHangup = Events #{const EPOLLRDHUP}

-- | The @EPOLLET@ event. Only appears in requests.
edgeTriggered :: Events Request
edgeTriggered = Events #{const EPOLLET}

-- | Does the first event set entirely contain the second one? That is,
-- is the second argument a subset of the first?
containsAllEvents :: Events e -> Events e -> Bool
containsAllEvents (Events a) (Events b) = a .&. b == b

-- | Does the first event set contain any of the events from the second one?
containsAnyEvents :: Events e -> Events e -> Bool
containsAnyEvents (Events a) (Events b) = (a .&. b) /= 0

sizeofEvent :: Int
sizeofEvent = #{size struct epoll_event}

-- | Read @events@ from @struct epoll_event@.
peekEventEvents :: Addr -> IO (Events e)
peekEventEvents (Addr p) = #{peek struct epoll_event, events} (Ptr p)

-- | Read @data.fd@ from @struct epoll_event@.
peekEventDataFd :: Addr -> IO Fd
peekEventDataFd (Addr p) = #{peek struct epoll_event, data.fd} (Ptr p)

-- | Read @data.ptr@ from @struct epoll_event@.
peekEventDataPtr :: Addr -> IO Addr
peekEventDataPtr (Addr p) = do
  Ptr q <- #{peek struct epoll_event, data.ptr} (Ptr p)
  pure (Addr q)

-- | Read @data.u32@ from @struct epoll_event@.
peekEventDataU32 :: Addr -> IO Word32
peekEventDataU32 (Addr p) = #{peek struct epoll_event, data.u32} (Ptr p)

-- | Read @data.u64@ from @struct epoll_event@.
peekEventDataU64 :: Addr -> IO Word64
peekEventDataU64 (Addr p) = #{peek struct epoll_event, data.u64} (Ptr p)

-- | Write @data.u64@ from @struct epoll_event@.
pokeEventDataU64 :: Addr -> Word64 -> IO ()
pokeEventDataU64 (Addr p) w = #{poke struct epoll_event, data.u64} (Ptr p) w

composePayload :: Word32 -> Word32 -> Word64
{-# inline composePayload #-}
composePayload a b = unsafeShiftL (word32ToWord64 a) 32 .|. word32ToWord64 b

decomposePayload :: Word64 -> (Word32,Word32)
{-# inline decomposePayload #-}
decomposePayload w = (word64ToWord32 (unsafeShiftR w 32), word64ToWord32 w)

word32ToWord64 :: Word32 -> Word64
word32ToWord64 = fromIntegral

word64ToWord32 :: Word64 -> Word32
word64ToWord32 = fromIntegral

unI :: Int -> Int##
unI (I## i) = i

-- -- | Read @data.u64@ from @struct epoll_event@.
-- readEventDataU64 ::
--      MutableByteArray RealWorld
--   -> Int -- ^ Index. Elements are @struct epoll_event@.
--   -> IO Word64
-- readEventDataU64 !arr !ix = do
--   -- On 64-bit platforms, Linux bitpacks this structure, causing the
--   -- data (a 64-bit word) to be misaligned. Consequently, we must
--   -- hardcode the assumed offsets to perform only aligned accesses.
--   -- The behavior is deterministic across platforms of different
--   -- endianness only if the only use of this function is paired with
--   -- writeEventDataU64.
--   (a :: Word32) <- PM.readByteArray arr (ix * 3 + 1)
--   (b :: Word32) <- PM.readByteArray arr (ix * 3 + 2)
--   pure (unsafeShiftL (word32ToWord64 a) 32 .|. word32ToWord64 b)
-- 
-- -- | Write @data.u64@ from @struct epoll_event@.
-- writeEventDataU64 ::
--      MutableByteArray RealWorld
--   -> Int -- ^ Index. Element are @struct epoll_event@.
--   -> Word64 -- ^ Data
--   -> IO ()
-- writeEventDataU64 !arr !ix !payload = do
--   -- See the comments on readEventDataU64
--   PM.writeByteArray arr (ix * 3 + 1) (word64ToWord32 (unsafeShiftR payload 32))
--   PM.writeByteArray arr (ix * 3 + 2) (word64ToWord32 payload)
