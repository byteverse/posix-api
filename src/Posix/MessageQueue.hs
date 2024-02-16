{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnliftedFFITypes #-}

module Posix.MessageQueue
  ( open
  , uninterruptibleReceiveByteArray
  , uninterruptibleSendBytes

    -- * Types
  , AccessMode (..)
  , CreationFlags (..)
  , StatusFlags (..)

    -- * Open Access Mode
  , F.readOnly
  , F.writeOnly
  , F.readWrite

    -- * Open Flags
  , F.nonblocking
  ) where

import qualified Control.Monad.Primitive as PM
import Data.Bits ((.|.))
import Data.Bytes.Types (Bytes (Bytes))
import Data.Primitive (ByteArray (..), MutableByteArray (..))
import qualified Data.Primitive as PM
import Foreign.C.Error (Errno, getErrno)
import Foreign.C.String (CString)
import Foreign.C.Types (CInt (..), CSize (..), CUInt (..))
import GHC.Exts (Addr#, ByteArray#, Int (I#), MutableByteArray#, RealWorld)
import qualified GHC.Exts as Exts
import Posix.File.Types (AccessMode (..), CreationFlags (..), StatusFlags (..))
import qualified Posix.File.Types as F
import System.Posix.Types (CSsize (..), Fd (..))

foreign import ccall unsafe "mqueue.h mq_receive"
  c_unsafe_mq_receive ::
    Fd ->
    MutableByteArray# RealWorld ->
    CSize ->
    Addr# ->
    IO CSsize

foreign import ccall unsafe "mqueue.h mq_send_offset"
  c_unsafe_mq_send_offset ::
    Fd ->
    ByteArray# ->
    Int ->
    CSize ->
    CUInt ->
    IO CInt

foreign import ccall safe "mqueue.h mq_open"
  c_safe_mq_open :: CString -> CInt -> IO Fd

open ::
  -- | NULL-terminated name of queue, must start with slash
  CString ->
  -- | Access mode
  AccessMode ->
  -- | Creation flags
  CreationFlags ->
  -- | Status flags
  StatusFlags ->
  IO (Either Errno Fd)
open !name (AccessMode x) (CreationFlags y) (StatusFlags z) =
  c_safe_mq_open name (x .|. y .|. z) >>= errorsFromFd

uninterruptibleReceiveByteArray ::
  -- | Message queue
  Fd ->
  -- | Maximum length of message
  CSize ->
  IO (Either Errno ByteArray)
uninterruptibleReceiveByteArray !fd !len = do
  m@(MutableByteArray m#) <- PM.newByteArray (csizeToInt len)
  r <- c_unsafe_mq_receive fd m# len Exts.nullAddr#
  case r of
    (-1) -> fmap Left getErrno
    _ -> do
      let sz = cssizeToInt r
      shrinkMutableByteArray m sz
      a <- PM.unsafeFreezeByteArray m
      pure (Right a)

uninterruptibleSendBytes ::
  -- | Message queue
  Fd ->
  -- | Message
  Bytes ->
  -- | Priority
  CUInt ->
  IO (Either Errno ())
uninterruptibleSendBytes !fd (Bytes (ByteArray arr) off len) pri =
  c_unsafe_mq_send_offset fd arr off (intToCSize len) pri
    >>= errorsFromInt_

shrinkMutableByteArray :: MutableByteArray RealWorld -> Int -> IO ()
shrinkMutableByteArray (MutableByteArray arr) (I# sz) =
  PM.primitive_ (Exts.shrinkMutableByteArray# arr sz)

cssizeToInt :: CSsize -> Int
cssizeToInt = fromIntegral

csizeToInt :: CSize -> Int
csizeToInt = fromIntegral

intToCSize :: Int -> CSize
intToCSize = fromIntegral

-- Sometimes, functions that return an int use zero to indicate
-- success and negative one to indicate failure without including
-- additional information in the value.
errorsFromInt_ :: CInt -> IO (Either Errno ())
errorsFromInt_ r =
  if r == 0
    then pure (Right ())
    else fmap Left getErrno

errorsFromFd :: Fd -> IO (Either Errno Fd)
errorsFromFd r =
  if r > (-1)
    then pure (Right r)
    else fmap Left getErrno
