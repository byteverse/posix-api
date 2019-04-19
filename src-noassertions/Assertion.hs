module Assertion
  ( assertMutablePrimArrayPinned
  ) where

import qualified Data.Primitive as PM

assertMutablePrimArrayPinned :: PM.MutablePrimArray s a -> PM.MutablePrimArray s a
assertMutablePrimArrayPinned = id
