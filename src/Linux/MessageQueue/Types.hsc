{-# language DataKinds #-}
{-# language DerivingStrategies #-}
{-# language DuplicateRecordFields #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language BinaryLiterals #-}
{-# language TypeApplications #-}

#include <mqueue.h>
module Linux.MessageQueue.Types
  ( -- * Open flags
    closeOnExec
  ) where

import Posix.MessageQueue.Types (OpenFlags(..))

-- | The @O_CLOEXEC@ open flag.
closeOnExec :: OpenFlags
closeOnExec = OpenFlags #{const O_CLOEXEC}
