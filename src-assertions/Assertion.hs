{-# LANGUAGE MagicHash #-}

module Assertion
  ( assertByteArrayPinned
  , assertMutableByteArrayPinned
  , assertMutablePrimArrayPinned
  ) where

import qualified Data.Primitive as PM

assertMutablePrimArrayPinned :: PM.MutablePrimArray s a -> PM.MutablePrimArray s a
assertMutablePrimArrayPinned x =
  if PM.isMutablePrimArrayPinned x
    then x
    else error "assertMutablePrimArrayPinned"

assertMutableByteArrayPinned :: PM.MutableByteArray s -> PM.MutableByteArray s
assertMutableByteArrayPinned x =
  if PM.isMutableByteArrayPinned x
    then x
    else error "assertMutableByteArrayPinned"

assertByteArrayPinned :: PM.ByteArray -> PM.ByteArray
assertByteArrayPinned x =
  if PM.isByteArrayPinned x
    then x
    else error "assertByteArrayPinned"
