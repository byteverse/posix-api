{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnliftedFFITypes #-}

module Posix.Poll
  ( uninterruptiblePoll
  , uninterruptiblePollMutablePrimArray
  , PollFd (..)
  , Exchange (..)
  , PT.input
  , PT.output
  , PT.error
  , PT.hangup
  , PT.invalid
  , PT.isSubeventOf
  ) where

import Data.Primitive (MutablePrimArray (..))
import Foreign.C.Error (Errno, getErrno)
import Foreign.C.Types (CInt (..))
import GHC.Exts (MutableByteArray#, RealWorld)
import GHC.Ptr (Ptr)
import Posix.Poll.Types (Exchange (..), PollFd (..))
import Posix.Types (CNfds (..))

import qualified Posix.Poll.Types as PT

foreign import ccall unsafe "poll.h poll"
  c_poll_ptr :: Ptr PollFd -> CNfds -> CInt -> IO CInt

foreign import ccall unsafe "poll.h poll"
  c_poll_prim_array :: MutableByteArray# RealWorld -> CNfds -> CInt -> IO CInt

{- | The @timeout@ argument is omitted since it is nonsense to choose
  anything other than 0 when using the unsafe FFI.
-}
uninterruptiblePoll ::
  Ptr PollFd ->
  CNfds ->
  IO (Either Errno CInt)
uninterruptiblePoll pfds n =
  c_poll_ptr pfds n 0 >>= errorsFromInt

uninterruptiblePollMutablePrimArray ::
  MutablePrimArray RealWorld PollFd ->
  CNfds ->
  IO (Either Errno CInt)
uninterruptiblePollMutablePrimArray (MutablePrimArray pfds) n =
  c_poll_prim_array pfds n 0 >>= errorsFromInt

errorsFromInt :: CInt -> IO (Either Errno CInt)
errorsFromInt r =
  if r >= 0
    then pure (Right r)
    else fmap Left getErrno
