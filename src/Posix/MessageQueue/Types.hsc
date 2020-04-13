#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
module Posix.MessageQueue.Types
  ( OpenMode(..)
  , OpenFlags(..)
    -- * Open Flags
  , nonblocking
  ) where

import Data.Bits ((.|.))
import Foreign.C.Types (CInt(..))

newtype OpenMode = OpenMode CInt

newtype OpenFlags = OpenFlags CInt

instance Semigroup OpenFlags where
  OpenFlags x <> OpenFlags y = OpenFlags (x .|. y)
instance Monoid OpenFlags where mempty = OpenFlags 0

-- | The @O_NONBLOCK@ open flag.
nonblocking :: OpenFlags
nonblocking = OpenFlags #{const O_NONBLOCK}
