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

#include <poll.h>

newtype CNfds = CNfds #{type nfds_t}
  deriving newtype (Eq,Real,Integral,Enum,Num,Ord,Storable,FiniteBits,Bits)
