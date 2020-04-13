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
  , CreationFlags(..)
  , AccessMode(..)
    -- * Open Access Mode
  , readOnly
  , writeOnly
  , readWrite
    -- * File Status Flags
  , nonblocking
  , append
    -- * File Creation Flags
  , create
  , truncate
  , exclusive
  ) where

import Prelude hiding (truncate)

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

-- | File Creation Flags
newtype CreationFlags = CreationFlags CInt
  deriving stock (Eq)
  deriving newtype (Bits)

newtype AccessMode = AccessMode CInt
  deriving stock (Eq)

instance Semigroup DescriptorFlags where (<>) = (.|.)
instance Monoid DescriptorFlags where mempty = DescriptorFlags 0

instance Semigroup CreationFlags where (<>) = (.|.)
instance Monoid CreationFlags where mempty = CreationFlags 0

instance Semigroup StatusFlags where (<>) = (.|.)
instance Monoid StatusFlags where mempty = StatusFlags 0

-- | The @O_RDONLY@ access mode.
readOnly :: AccessMode
readOnly = AccessMode #{const O_RDONLY}

-- | The @O_WRONLY@ access mode.
writeOnly :: AccessMode
writeOnly = AccessMode #{const O_WRONLY}

-- | The @O_RDWR@ access mode.
readWrite :: AccessMode
readWrite = AccessMode #{const O_RDWR}

-- | The @O_NONBLOCK@ flag
nonblocking :: StatusFlags
nonblocking = StatusFlags #{const O_NONBLOCK}

-- | The @O_APPEND@ flag
append :: StatusFlags
append = StatusFlags #{const O_APPEND}

-- | The @O_CREAT@ flag
create :: CreationFlags
create = CreationFlags #{const O_CREAT}

-- | The @O_TRUNC@ flag
truncate :: CreationFlags
truncate = CreationFlags #{const O_TRUNC}

-- | The @O_EXCL@ flag
exclusive :: CreationFlags
exclusive = CreationFlags #{const O_EXCL}
