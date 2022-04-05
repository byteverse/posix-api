module Assertion
  ( assertByteArrayPinned
  , assertMutableByteArrayPinned
  , assertMutablePrimArrayPinned
  ) where

import qualified Data.Primitive as PM

assertMutablePrimArrayPinned :: PM.MutablePrimArray s a -> PM.MutablePrimArray s a
assertMutablePrimArrayPinned = id

assertMutableByteArrayPinned :: PM.MutableByteArray s -> PM.MutableByteArray s
assertMutableByteArrayPinned = id

assertByteArrayPinned :: PM.ByteArray -> PM.ByteArray
assertByteArrayPinned = id
