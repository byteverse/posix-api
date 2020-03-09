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
  ( DescriptorFlags(..)
  , StatusFlags(..)
    -- * File Status Flags
  , nonblocking
  , append
  ) where

import Data.Bits (Bits,(.|.))
import Foreign.C.Types (CInt)

-- | File Descriptor Flags
newtype DescriptorFlags = DescriptorFlags CInt
  deriving stock (Eq)
  deriving newtype (Bits)

-- | File Status Flags
newtype StatusFlags = StatusFlags CInt
  deriving stock (Eq)
  deriving newtype (Bits)

instance Semigroup DescriptorFlags where (<>) = (.|.)
instance Monoid DescriptorFlags where mempty = DescriptorFlags 0

instance Semigroup StatusFlags where (<>) = (.|.)
instance Monoid StatusFlags where mempty = StatusFlags 0

-- | The @O_NONBLOCK@ flag
nonblocking :: StatusFlags
nonblocking = StatusFlags #{const O_NONBLOCK}

-- | The @O_APPEND@ flag
append :: StatusFlags
append = StatusFlags #{const O_APPEND}
