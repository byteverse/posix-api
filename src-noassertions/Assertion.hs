module Assertion
  ( assertByteArrayPinned
  , assertMutablePrimArrayPinned
  ) where

import qualified Data.Primitive as PM

assertMutablePrimArrayPinned :: PM.MutablePrimArray s a -> PM.MutablePrimArray s a
assertMutablePrimArrayPinned = id

assertByteArrayPinned :: PM.ByteArray -> PM.ByteArray
assertByteArrayPinned = id
