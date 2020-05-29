{-# language BangPatterns #-}
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
  , fromLatinString
  , pinnedFromBytes
  , pin
  , touch
  , contents
  ) where

import Control.Monad.ST (ST)
import Control.Monad.ST.Run (runByteArrayST)
import Data.Bytes.Types (Bytes(Bytes))
import Data.Char (ord)
import Data.Primitive (ByteArray(..),MutableByteArray)
import Data.Word (Word8)
import Foreign.C.String (CString)
import Foreign.Ptr (castPtr)
import GHC.Exts (Int(I#),Char(C#),ByteArray#,chr#,touch#)
import GHC.IO (IO(IO))

import qualified Data.Bytes as Bytes
import qualified Data.Primitive as PM
import qualified GHC.Exts as Exts

-- | An unsliced byte sequence with @NUL@ as the final byte.
newtype ManagedCString = ManagedCString ByteArray

instance Exts.IsString ManagedCString where
  fromString = fromLatinString

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

-- | Convert a 'String' consisting of only characters representable
-- by ISO-8859-1. These are encoded with ISO-8859-1. Any character
-- with a codepoint above @U+00FF@ is replaced by an unspecified byte.
fromLatinString :: String -> ManagedCString
{-# noinline fromLatinString #-}
fromLatinString str = ManagedCString $ runByteArrayST $ do
  let lenPred0 = 63
  dst0 <- PM.newByteArray (lenPred0 + 1)
  go str dst0 0 lenPred0
  where
  go :: forall s. String -> MutableByteArray s -> Int -> Int -> ST s ByteArray
  go [] !dst !ix !_ = do
    PM.writeByteArray dst ix (0 :: Word8)
    PM.resizeMutableByteArray dst (ix + 1) >>= PM.unsafeFreezeByteArray
  go (c:cs) !dst !ix !lenPred = if ix < lenPred
    then do
      PM.writeByteArray dst ix (fromIntegral @Int @Word8 (ord c))
      go cs dst (ix + 1) lenPred
    else do
      let nextLenPred = lenPred * 2
      dst' <- PM.newByteArray (nextLenPred + 1)
      PM.copyMutableByteArray dst' 0 dst 0 ix
      PM.writeByteArray dst' ix (fromIntegral @Int @Word8 (ord c))
      go cs dst' (ix + 1) nextLenPred

-- | Get a pointer to the payload of the managed C string. The behavior is
-- undefined if the argument is not pinned.
contents :: ManagedCString -> CString
contents (ManagedCString x) = castPtr (PM.byteArrayContents x)
