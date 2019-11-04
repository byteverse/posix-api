#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
module Posix.MessageQueue.Types
  ( OpenMode(..)
  , OpenFlags(..)
  , applyOpenFlags
    -- * Open Access Mode
  , readOnly
  , writeOnly
  , readWrite
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

-- | The @O_RDONLY@ access mode.
readOnly :: OpenMode
readOnly = OpenMode #{const O_RDONLY}

-- | The @O_WRONLY@ access mode.
writeOnly :: OpenMode
writeOnly = OpenMode #{const O_WRONLY}

-- | The @O_RDWR@ access mode.
readWrite :: OpenMode
readWrite = OpenMode #{const O_RDWR}

-- | The @O_NONBLOCK@ open flag.
nonblocking :: OpenFlags
nonblocking = OpenFlags #{const O_NONBLOCK}

applyOpenFlags :: OpenFlags -> OpenMode -> OpenMode
applyOpenFlags (OpenFlags s) (OpenMode t) = OpenMode (s .|. t)

