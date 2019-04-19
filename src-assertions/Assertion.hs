{-# language MagicHash #-}
{-# language UnboxedTuples #-}

module Assertion
  ( assertMutablePrimArrayPinned
  ) where

import GHC.Exts (isTrue#)

import qualified Data.Primitive as PM
import qualified GHC.Exts as Exts

assertMutablePrimArrayPinned :: PM.MutablePrimArray s a -> PM.MutablePrimArray s a
assertMutablePrimArrayPinned x = if isMutablePrimArrayPinned x
  then x
  else error "assertMutablePrimArrayPinned"

isMutablePrimArrayPinned :: PM.MutablePrimArray s a -> Bool
isMutablePrimArrayPinned (PM.MutablePrimArray marr#) = isTrue# (Exts.isMutableByteArrayPinned# marr#)
