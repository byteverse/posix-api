{-# language BangPatterns #-}
{-# language DataKinds #-}
{-# language GADTSyntax #-}
{-# language KindSignatures #-}
{-# language MagicHash #-}
{-# language UnliftedFFITypes #-}
{-# language NamedFieldPuns #-}
{-# language UnboxedTuples #-}

module Linux.Epoll
  ( -- * Functions
    -- ** Create
    uninterruptibleCreate
  , uninterruptibleCreate1
    -- ** Wait
  , waitMutablePrimArray
  , uninterruptibleWaitMutablePrimArray
    -- ** Control
  , uninterruptibleControlMutablePrimArray
    -- * Types
  , EpollFlags(..)
  , ControlOperation(..)
  , Events(..)
  , Event(..)
  , Exchange(..)
    -- * Classes
  , PrimEpollData
    -- * Constants
  , T.closeOnExec
  , T.add
  , T.modify
  , T.delete
  , T.input
  , T.output
  , T.priority
  , T.hangup
  , T.readHangup
  , T.error
  , T.edgeTriggered
    -- * Events Combinators
  , T.containsEvents
    -- * Marshalling
  , T.sizeofEvent
  , T.peekEventEvents
  , T.peekEventDataFd
  , T.peekEventDataPtr
  , T.peekEventDataU32
  , T.peekEventDataU64
  , T.pokeEventDataU64
  -- , T.readEventDataU64
  -- , T.writeEventDataU64
  -- , T.writeEventEvents
  ) where

import Prelude hiding (error)

import Data.Primitive (MutablePrimArray(..))
import Foreign.C.Error (Errno,getErrno)
import Foreign.C.Types (CInt(..))
import GHC.Exts (RealWorld,MutableByteArray#)
import Linux.Epoll.Types (EpollFlags(..),ControlOperation(..),Events(..),Exchange(..))
import Linux.Epoll.Types (Event(..),PrimEpollData(..))
import System.Posix.Types (Fd(..))

import qualified Linux.Epoll.Types as T

foreign import ccall unsafe "sys/epoll.h epoll_create"
  c_epoll_create :: CInt -> IO Fd

foreign import ccall unsafe "sys/epoll.h epoll_create1"
  c_epoll_create1 :: EpollFlags -> IO Fd

foreign import ccall unsafe "sys/epoll.h epoll_wait"
  c_epoll_wait :: Fd -> MutableByteArray# RealWorld -> CInt -> CInt -> IO CInt

foreign import ccall safe "sys/epoll.h epoll_wait"
  c_epoll_wait_safe :: Fd -> MutableByteArray# RealWorld -> CInt -> CInt -> IO CInt

foreign import ccall unsafe "sys/epoll.h epoll_ctl"
  c_epoll_ctl :: Fd -> ControlOperation -> Fd -> MutableByteArray# RealWorld -> IO CInt

-- -- | Write @data.u64@ from @struct epoll_event@.
-- writeEventEvents ::
--      MutableByteArray RealWorld
--   -> Int -- ^ Index. Element are @struct epoll_event@.
--   -> Events e
--   -> IO ()
-- writeEventEvents !arr !ix !payload = do
--   -- See the comments on readEventDataU64
--   PM.writeByteArray arr (ix * 3 + 1) (word64ToWord32 (unsafeShiftR payload 32))
--   PM.writeByteArray arr (ix * 3 + 2) (word64ToWord32 payload)

uninterruptibleCreate ::
     CInt -- ^ Size, ignored since Linux 2.6.8
  -> IO (Either Errno Fd)
uninterruptibleCreate !sz = c_epoll_create sz >>= errorsFromFd

uninterruptibleCreate1 ::
     EpollFlags -- ^ Flags
  -> IO (Either Errno Fd)
uninterruptibleCreate1 !flags =
  c_epoll_create1 flags >>= errorsFromFd

-- | Wait for an I/O event on an epoll file descriptor. The
--   <https://linux.die.net/man/2/epoll_wait Linux man page>
--   includes more details. The @timeout@ argument is omitted
--   since it is nonsense to choose anything other than 0 when
--   using the unsafe FFI.
uninterruptibleWaitMutablePrimArray ::
     Fd -- ^ EPoll file descriptor
  -> MutablePrimArray RealWorld (Event 'Response a) -- ^ Event buffer
  -> CInt -- ^ Maximum events
  -> IO (Either Errno CInt) -- ^ Number of events received
uninterruptibleWaitMutablePrimArray !epfd (MutablePrimArray evs) !maxEvents =
  c_epoll_wait epfd evs maxEvents 0 >>= errorsFromInt

waitMutablePrimArray ::
     Fd -- ^ EPoll file descriptor
  -> MutablePrimArray RealWorld (Event 'Response a) -- ^ Event buffer
  -> CInt -- ^ Maximum events
  -> CInt -- ^ Timeout in milliseconds, use @-1@ to block forever.
  -> IO (Either Errno CInt) -- ^ Number of events received
waitMutablePrimArray !epfd (MutablePrimArray evs) !maxEvents !timeout =
  c_epoll_wait_safe epfd evs maxEvents timeout >>= errorsFromInt

-- | Add, modify, or remove entries in the interest list of the
--   epoll instance referred to by the file descriptor @epfd@.
--   <https://linux.die.net/man/2/epoll_ctl Linux man page>
--   includes more details.
uninterruptibleControlMutablePrimArray ::
     Fd -- ^ EPoll file descriptor (@epfd@)
  -> ControlOperation
     -- ^ Operation: @EPOLL_CTL_ADD@, @EPOLL_CTL_MOD@, or @EPOLL_CTL_DEL@
  -> Fd -- ^ File descriptor whose registration will be affected
  -> MutablePrimArray RealWorld (Event 'Request a)
     -- ^ A single event. This is read from, not written to.
  -> IO (Either Errno ())
uninterruptibleControlMutablePrimArray !epfd !op !fd (MutablePrimArray ev) =
  c_epoll_ctl epfd op fd ev >>= errorsFromInt_

errorsFromFd :: Fd -> IO (Either Errno Fd)
errorsFromFd r = if r > (-1)
  then pure (Right r)
  else fmap Left getErrno

errorsFromInt :: CInt -> IO (Either Errno CInt)
errorsFromInt r = if r >= 0
  then pure (Right r)
  else fmap Left getErrno

-- Sometimes, functions that return an int use zero to indicate
-- success and negative one to indicate failure without including
-- additional information in the value.
errorsFromInt_ :: CInt -> IO (Either Errno ())
errorsFromInt_ r = if r == 0
  then pure (Right ())
  else fmap Left getErrno

