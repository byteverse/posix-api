{-# language MagicHash #-}
{-# language MultiWayIf #-}
{-# language TypeApplications #-}
{-# language ScopedTypeVariables #-}

module Foreign.C.String.Managed
  ( ManagedCString(..)
  , terminated
  , terminatedU
  , unterminated
  ) where

import Data.Bytes.Types (Bytes(Bytes))
import Data.Primitive (ByteArray)
import Data.Word (Word8)
import GHC.Exts (Int(I#),Char(C#),chr#)

import qualified Data.Primitive as PM
import qualified Data.Bytes as Bytes

newtype ManagedCString = ManagedCString ByteArray

instance Show ManagedCString where
  showsPrec _ (ManagedCString arr) s0 = PM.foldrByteArray
    ( \(w :: Word8) s ->
      if | w == 0 -> s
         | w < 32 -> '?' : s
         | w > 126 -> '?' : s
         | otherwise -> case fromIntegral @Word8 @Int w of
             I# i -> C# (chr# i) : s
    ) s0 arr

terminatedU :: ManagedCString -> ByteArray
terminatedU (ManagedCString x) = x

terminated :: ManagedCString -> Bytes
terminated (ManagedCString x) = Bytes.fromByteArray x

unterminated :: ManagedCString -> Bytes
unterminated (ManagedCString x) = Bytes x 0 (PM.sizeofByteArray x - 1)
