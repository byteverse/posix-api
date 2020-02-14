{-# language BangPatterns #-}

module Posix.File
  ( -- * Functions
    getFdFlags
    -- * Types
  , FdFlags(..)
    -- * File Descriptor Flags
  , Types.nonblocking
  ) where

import Posix.File.Types (FdFlags(..))
import System.Posix.Types (Fd(..))
import Foreign.C.Error (Errno,getErrno)
import Foreign.C.Types (CInt(..))

import qualified Posix.File.Types as Types

-- | Get file descriptor flags. This uses the unsafe FFI to perform @fcntl(fd,F_GETFD)@.
getFdFlags :: Fd -> IO (Either Errno FdFlags)
getFdFlags !fd = c_getFdFlags fd >>= errorsFromFdFlags

foreign import ccall unsafe "HaskellPosix.h hs_get_fd_flags"
  c_getFdFlags :: Fd -> IO FdFlags

errorsFromFdFlags :: FdFlags -> IO (Either Errno FdFlags)
errorsFromFdFlags r@(FdFlags x) = if x > (-1)
  then pure (Right r)
  else fmap Left getErrno
