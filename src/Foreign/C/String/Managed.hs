{-# language MagicHash #-}
{-# language MultiWayIf #-}
{-# language TypeApplications #-}
{-# language ScopedTypeVariables #-}
{-# language UnboxedTuples #-}

module Foreign.C.String.Managed
  ( ManagedCString(..)
  , terminated
  , terminatedU
  , unterminated
  , fromBytes
  , pinnedFromBytes
  , pin
  , touch
  ) where

import Control.Monad.ST.Run (runByteArrayST)
import Data.Primitive (ByteArray(..))
import Data.Bytes.Types (Bytes(Bytes))
import Data.Primitive (ByteArray)
import Data.Word (Word8)
import GHC.Exts (Int(I#),Char(C#),ByteArray#,chr#,touch#)
import GHC.IO (IO(IO))

import qualified Data.Primitive as PM
import qualified Data.Bytes as Bytes

-- | An unsliced byte sequence with @NUL@ as the final byte.
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

-- | Copies the slice, appending a @NUL@ byte to the end.
fromBytes :: Bytes -> ManagedCString
fromBytes (Bytes arr off len) = ManagedCString $ runByteArrayST $ do
  dst <- PM.newByteArray (len + 1)
  PM.copyByteArray dst 0 arr off len
  PM.writeByteArray dst len (0 :: Word8)
  PM.unsafeFreezeByteArray dst

-- | Copies the slice into pinned memory, appending a @NUL@ byte to the end.
pinnedFromBytes :: Bytes -> ManagedCString
pinnedFromBytes (Bytes arr off len) = ManagedCString $ runByteArrayST $ do
  dst <- PM.newPinnedByteArray (len + 1)
  PM.copyByteArray dst 0 arr off len
  PM.writeByteArray dst len (0 :: Word8)
  PM.unsafeFreezeByteArray dst

pin :: ManagedCString -> ManagedCString
pin (ManagedCString x) = if PM.isByteArrayPinned x
  then ManagedCString x
  else ManagedCString $ runByteArrayST $ do
    let len = PM.sizeofByteArray x
    dst <- PM.newPinnedByteArray len
    PM.copyByteArray dst 0 x 0 len
    PM.unsafeFreezeByteArray dst

touch :: ManagedCString -> IO ()
touch (ManagedCString (ByteArray x)) = touchByteArray# x

touchByteArray# :: ByteArray# -> IO ()
touchByteArray# x = IO $ \s -> case touch# x s of s' -> (# s', () #)
