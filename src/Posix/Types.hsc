{-# language DataKinds #-}
{-# language DerivingStrategies #-}
{-# language DuplicateRecordFields #-}
{-# language GeneralizedNewtypeDeriving #-}

module Posix.Types
  ( CNfds(..)
  ) where

import System.Posix.Types (CNfds(..))

#include <poll.h>
