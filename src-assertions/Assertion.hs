{-# language MagicHash #-}
{-# language UnboxedTuples #-}

module Assertion
  ( assertByteArrayPinned
  , assertMutableByteArrayPinned
  , assertMutablePrimArrayPinned
  ) where

import GHC.Exts (isTrue#)

import qualified Data.Primitive as PM
import qualified GHC.Exts as Exts

assertMutablePrimArrayPinned :: PM.MutablePrimArray s a -> PM.MutablePrimArray s a
assertMutablePrimArrayPinned x = if isMutablePrimArrayPinned x
  then x
  else error "assertMutablePrimArrayPinned"

assertMutableByteArrayPinned :: PM.MutablePrimArray s a -> PM.MutablePrimArray s a
assertMutableByteArrayPinned x = if isMutableByteArrayPinned x
  then x
  else error "assertMutableByteArrayPinned"

isMutablePrimArrayPinned :: PM.MutablePrimArray s a -> Bool
{-# inline isMutablePrimArrayPinned #-}
isMutablePrimArrayPinned (PM.MutablePrimArray marr#) = isTrue# (Exts.isMutableByteArrayPinned# marr#)

isMutableByteArrayPinned :: PM.MutableByteArray s -> Bool
{-# inline isMutableByteArrayPinned #-}
isMutableByteArrayPinned (PM.MutableByteArray marr#) = isTrue# (Exts.isMutableByteArrayPinned# marr#)

assertByteArrayPinned :: PM.ByteArray -> PM.ByteArray
assertByteArrayPinned x = if PM.isByteArrayPinned x
  then x
  else error "assertByteArrayPinned"
