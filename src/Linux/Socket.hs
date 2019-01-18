module Linux.Socket
  ( -- * Types
    SocketFlags(..)
    -- * Message Flags
  , LST.dontWait
  , LST.truncate
    -- * Socket Flags
  , LST.closeOnExec
  , LST.nonblocking
    -- * Twiddle
  , applySocketFlags
  ) where

import Prelude hiding (truncate)

import Data.Bits ((.|.))
import Linux.Socket.Types (SocketFlags(..))
import Posix.Socket (Type(..))

import qualified Linux.Socket.Types as LST

-- | Linux extends the @type@ argument of
--   <http://man7.org/linux/man-pages/man2/socket.2.html socket> to accept
--   flags. It is advisable to set @SOCK_CLOEXEC@ on when opening a socket
--   on linux. For example, we may open a TCP Internet socket with:
--
--   > uninterruptibleSocket internet (applySocketFlags closeOnExec stream) defaultProtocol
--
--   To additionally open the socket in nonblocking mode
--   (e.g. with @SOCK_NONBLOCK@):
--
--   > uninterruptibleSocket internet (applySocketFlags (closeOnExec <> nonblocking) stream) defaultProtocol
--   
applySocketFlags :: SocketFlags -> Type -> Type
applySocketFlags (SocketFlags s) (Type t) = Type (s .|. t)
