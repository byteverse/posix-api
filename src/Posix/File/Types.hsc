{-# language DataKinds #-}
{-# language DerivingStrategies #-}
{-# language DuplicateRecordFields #-}
{-# language GADTSyntax #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language KindSignatures #-}
{-# language MagicHash #-}
{-# language UnboxedTuples #-}
{-# language NamedFieldPuns #-}

-- This is needed because hsc2hs does not currently handle ticked
-- promoted data constructors correctly.
{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

#include <fcntl.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>

-- | All of the data constructors provided by this module are unsafe.
--   Only use them if you really know what you are doing.
module Posix.File.Types
  ( FdFlags(..)
    -- * File Descriptor Flags
  , nonblocking
  ) where

import Data.Bits (Bits,(.|.))
import Foreign.C.Types (CInt)

-- | File Descriptor Flags
newtype FdFlags = FdFlags CInt
  deriving stock (Eq)
  deriving newtype (Bits)

instance Semigroup FdFlags where (<>) = (.|.)
instance Monoid FdFlags where mempty = FdFlags 0

-- | The @O_NONBLOCK@ flag
nonblocking :: FdFlags
nonblocking = FdFlags #{const O_NONBLOCK}
