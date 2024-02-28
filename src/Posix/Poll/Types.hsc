{-# language BangPatterns #-}
{-# language BinaryLiterals #-}
{-# language DataKinds #-}
{-# language DerivingStrategies #-}
{-# language DuplicateRecordFields #-}
{-# language GADTSyntax #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language KindSignatures #-}
{-# language NamedFieldPuns #-}
{-# language TypeApplications #-}
{-# language MagicHash #-}
{-# language UnboxedTuples #-}
{-# language PolyKinds #-}
{-# language DataKinds #-}

-- This is needed because hsc2hs does not currently handle ticked
-- promoted data constructors correctly.
{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

#define _GNU_SOURCE
#include <poll.h>
#include "custom.h"

-- | All of the data constructors provided by this module are unsafe.
--   Only use them if you really know what you are doing.
module Posix.Poll.Types
  ( PollFd(..)
  , Exchange(..)
  , input
  , output
  , error
  , hangup
  , invalid
  , isSubeventOf
  ) where

import Prelude hiding (truncate,error)

import Data.Bits ((.|.),(.&.))
import Data.Primitive (Prim(..))
import Foreign.C.Types (CInt(..),CShort)
import Foreign.Storable (Storable(..))
import GHC.Exts (Int(I##),Int##,(+##),(*##))
import System.Posix.Types (Fd(..))

import qualified Data.Kind
import qualified Data.Primitive as PM

data PollFd = PollFd
  { descriptor :: !Fd
    -- ^ The @fd@ field of @struct pollfd@
  , request :: !(Event Request)
    -- ^ The @events@ field of @struct pollfd@
  , response :: !(Event Response)
    -- ^ The @revents@ field of @struct pollfd@
  }

newtype Event :: Exchange -> Data.Kind.Type where
  Event :: CShort -> Event e
  deriving newtype (Eq,Storable,Prim)

instance Semigroup (Event e) where
  Event a <> Event b = Event (a .|. b)

instance Monoid (Event e) where
  mempty = Event 0

data Exchange = Request | Response

instance Storable PollFd where
  sizeOf _ = #{size struct pollfd}
  alignment _ = alignment (undefined :: CInt)
  peek ptr = do
    descriptor <- #{peek struct pollfd, fd} ptr
    request <- #{peek struct pollfd, events} ptr
    response <- #{peek struct pollfd, revents} ptr
    let !pollfd = PollFd{descriptor,request,response}
    pure pollfd
  poke ptr PollFd{descriptor,request,response} = do
    #{poke struct pollfd, fd} ptr descriptor
    #{poke struct pollfd, events} ptr request
    #{poke struct pollfd, revents} ptr response

unI :: Int -> Int##
unI (I## i) = i

instance Prim PollFd where
  sizeOf## _ = unI #{size struct pollfd}
  alignment## _ = alignment## (undefined :: CInt)
  indexByteArray## arr i = PollFd
    { descriptor = #{indexByteArrayHash struct pollfd, fd} arr i
    , request = #{indexByteArrayHash struct pollfd, events} arr i
    , response = #{indexByteArrayHash struct pollfd, revents} arr i
    }
  writeByteArray## arr i PollFd{descriptor,request,response} s0 = case #{writeByteArrayHash struct pollfd, fd} arr i descriptor s0 of
    s1 -> case #{writeByteArrayHash struct pollfd, events} arr i request s1 of
      s2 -> #{writeByteArrayHash struct pollfd, revents} arr i response s2
  readByteArray## arr i s0 = case #{readByteArrayHash struct pollfd, fd} arr i s0 of
    (## s1, descriptor ##) -> case #{readByteArrayHash struct pollfd, events} arr i s1 of
      (## s2, request ##) -> case #{readByteArrayHash struct pollfd, revents} arr i s2 of
        (## s3, response ##) -> (## s3, PollFd{descriptor,request,response} ##)
  setByteArray## = PM.defaultSetByteArray##
  indexOffAddr## arr i = PollFd
    { descriptor = #{indexOffAddrHash struct pollfd, fd} arr i
    , request = #{indexOffAddrHash struct pollfd, events} arr i
    , response = #{indexOffAddrHash struct pollfd, revents} arr i
    }
  writeOffAddr## arr i PollFd{descriptor,request,response} s0 = case #{writeOffAddrHash struct pollfd, fd} arr i descriptor s0 of
    s1 -> case #{writeOffAddrHash struct pollfd, events} arr i request s1 of
      s2 -> #{writeOffAddrHash struct pollfd, revents} arr i response s2
  readOffAddr## arr i s0 = case #{readOffAddrHash struct pollfd, fd} arr i s0 of
    (## s1, fdVal ##) -> case #{readOffAddrHash struct pollfd, events} arr i s1 of
      (## s2, eventsVal ##) -> case #{readOffAddrHash struct pollfd, revents} arr i s2 of
        (## s3, reventsVal ##) -> (## s3, PollFd fdVal eventsVal reventsVal ##)
  setOffAddr## = PM.defaultSetOffAddr##

-- | The @POLLIN@ event.
input :: Event e
input = Event #{const POLLIN}

-- | The @POLLOUT@ event.
output :: Event e
output = Event #{const POLLOUT}

-- | The @POLLERR@ event.
error :: Event Response
error = Event #{const POLLERR}

-- | The @POLLHUP@ event.
hangup :: Event Response
hangup = Event #{const POLLHUP}

-- | The @POLLNVAL@ event.
invalid :: Event Response
invalid = Event #{const POLLNVAL}

-- | Is the first argument a subset of the second argument?
isSubeventOf :: Event e -> Event e -> Bool
isSubeventOf (Event a) (Event b) = a .&. b == a
