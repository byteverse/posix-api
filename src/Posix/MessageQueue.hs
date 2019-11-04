{-# language BangPatterns #-}
{-# language MagicHash #-}
{-# language UnboxedTuples #-}
{-# language UnliftedFFITypes #-}
module Posix.MessageQueue
  ( open
  , uninterruptibleReceiveByteArray
  , uninterruptibleSendBytes
    -- * Types
  , OpenMode(..)
  , OpenFlags(..)
    -- * Bit Twiddle
  , T.applyOpenFlags
    -- * Open Access Mode
  , T.readOnly
  , T.writeOnly
  , T.readWrite
    -- * Open Flags
  , T.nonblocking
  ) where

import GHC.Exts (RealWorld,ByteArray#,MutableByteArray#,Addr#)
import GHC.Exts (Int(I#))
import System.Posix.Types (Fd(..),CSsize(..))
import Foreign.C.Types (CInt(..),CSize(..),CUInt(..))
import Foreign.C.Error (Errno,getErrno)
import Foreign.C.String (CString)
import Data.Primitive (MutableByteArray(..),ByteArray(..))
import Data.Bytes.Types (Bytes(Bytes))
import Posix.MessageQueue.Types (OpenMode(..),OpenFlags(..))
import qualified GHC.Exts as Exts
import qualified Data.Primitive as PM
import qualified Control.Monad.Primitive as PM
import qualified Posix.MessageQueue.Types as T

foreign import ccall unsafe "mqueue.h mq_receive"
  c_unsafe_mq_receive :: Fd -> MutableByteArray# RealWorld
                      -> CSize -> Addr# -> IO CSsize

foreign import ccall unsafe "mqueue.h mq_send_offset"
  c_unsafe_mq_send_offset :: Fd
    -> ByteArray# -> Int -> CSize -> CUInt -> IO CInt

foreign import ccall safe "mqueue.h mq_open"
  c_safe_mq_open :: CString -> OpenMode -> IO Fd

open ::
     CString -- ^ NULL-terminated name of queue, must start with slash
  -> OpenMode -- ^ Access mode and flags
  -> IO (Either Errno Fd)
open !name !mode =
  c_safe_mq_open name mode >>= errorsFromFd

uninterruptibleReceiveByteArray ::
     Fd -- ^ Message queue
  -> CSize -- ^ Maximum length of message
  -> IO (Either Errno ByteArray)
uninterruptibleReceiveByteArray !fd !len = do
  m@(MutableByteArray m# ) <- PM.newByteArray (csizeToInt len)
  r <- c_unsafe_mq_receive fd m# len Exts.nullAddr# 
  case r of
    (-1) -> fmap Left getErrno
    _ -> do
      let sz = cssizeToInt r
      shrinkMutableByteArray m sz
      a <- PM.unsafeFreezeByteArray m
      pure (Right a)

uninterruptibleSendBytes ::
     Fd -- ^ Message queue
  -> Bytes -- ^ Message
  -> CUInt -- ^ Priority
  -> IO (Either Errno ())
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
errorsFromInt_ r = if r == 0
  then pure (Right ())
  else fmap Left getErrno

errorsFromFd :: Fd -> IO (Either Errno Fd)
errorsFromFd r = if r > (-1)
  then pure (Right r)
  else fmap Left getErrno

