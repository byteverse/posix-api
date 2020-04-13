{-# language DataKinds #-}
{-# language DerivingStrategies #-}
{-# language DuplicateRecordFields #-}
{-# language GeneralizedNewtypeDeriving #-}

module Posix.Types
  ( CNfds(..)
  ) where

import Data.Word

import Foreign.Storable (Storable)
import Data.Bits (FiniteBits,Bits)
#if MIN_VERSION_base(4,12,0)
import System.Posix.Types (CNfds(..))
#endif

#include <poll.h>

-- This is a compatibility shim for older GHCs
#if !MIN_VERSION_base(4,12,0)
newtype CNfds = CNfds #{type nfds_t}
  deriving newtype (Eq,Real,Integral,Enum,Num,Ord,Storable,FiniteBits,Bits)
#endif
