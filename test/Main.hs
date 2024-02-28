{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

import Control.Concurrent (forkIO, threadWaitWrite)
import Control.Monad (when)
import Data.Primitive (ByteArray, MutableByteArray (..), MutablePrimArray (..))
import Data.Word (Word8)
import Foreign.C.Error (Errno, errnoToIOError)
import Foreign.C.Types (CSize)
import GHC.Exts (RealWorld)
import Numeric (showIntAtBase)
import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.Primitive as PM
import qualified Data.Primitive.MVar as PM
import qualified GHC.Exts as E
import qualified Linux.Epoll as Epoll
import qualified Posix.Socket as S

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "tests"
    [ testGroup
        "posix"
        [ testGroup
            "sockets"
            [ testCase "A" testSocketsA
            , testCase "B" testSocketsB
            , testCase "C" testSocketsC
            , testCase "D" testSocketsD
            ]
        ]
    , testGroup
        "linux"
        [ testGroup
            "epoll"
            [ testCase "A" testLinuxEpollA
            ]
        ]
    ]

testSocketsA :: Assertion
testSocketsA = do
  (a, b) <- demand =<< S.uninterruptibleSocketPair S.Unix S.datagram S.defaultProtocol
  m <- PM.newEmptyMVar
  _ <- forkIO $ S.receiveByteArray b 5 mempty >>= PM.putMVar m
  bytesSent <- demand =<< S.sendByteArray a sample 0 5 mempty
  when (bytesSent /= 5) (fail "testSocketsA: bytesSent was wrong")
  actual <- demand =<< PM.takeMVar m
  sample @=? actual

testSocketsB :: Assertion
testSocketsB = do
  let limit = 10
      wordSz = PM.sizeOf (undefined :: Int)
      cwordSz = fromIntegral wordSz :: CSize
  (a, b) <- demand =<< S.uninterruptibleSocketPair S.Unix S.datagram S.defaultProtocol
  lock <- PM.newEmptyMVar
  let go1 !(ix :: Int) !(n :: Int) =
        if (ix < limit)
          then do
            y <- PM.newByteArray wordSz
            PM.writeByteArray y 0 (1 + n)
            z <- PM.unsafeFreezeByteArray y
            oneWord =<< demand =<< S.sendByteArray b z 0 cwordSz mempty
            x <- demand =<< S.receiveByteArray b cwordSz mempty
            go1 (ix + 1) (PM.indexByteArray x 0)
          else pure n
      go2 !(ix :: Int) =
        if (ix < limit)
          then do
            x <- demand =<< S.receiveByteArray a cwordSz mempty
            y <- PM.newByteArray wordSz
            PM.writeByteArray y 0 (1 + PM.indexByteArray x 0 :: Int)
            z <- PM.unsafeFreezeByteArray y
            oneWord =<< demand =<< S.sendByteArray a z 0 cwordSz mempty
            go2 (ix + 1)
          else PM.putMVar lock ()
  _ <- forkIO (go2 0)
  r <- go1 0 0
  PM.takeMVar lock
  20 @=? r

testSocketsC :: Assertion
testSocketsC = do
  (a, b) <- demand =<< S.uninterruptibleSocketPair S.Unix S.datagram S.defaultProtocol
  m <- PM.newEmptyMVar
  _ <- forkIO $ S.receiveByteArray a 5 mempty >>= PM.putMVar m
  bytesSent <- demand =<< S.sendByteArray b sample 0 5 mempty
  when (bytesSent /= 5) (fail "testSocketsC: bytesSent was wrong")
  actual <- demand =<< PM.takeMVar m
  sample @=? actual

testSocketsD :: Assertion
testSocketsD = do
  (a, b) <- demand =<< S.uninterruptibleSocketPair S.Unix S.datagram S.defaultProtocol
  _ <- forkIO $ do
    bytesSent <- demand =<< S.sendByteArray b sample 0 5 mempty
    when (bytesSent /= 5) (fail "testSocketsD: bytesSent was wrong")
  actual <- demand =<< S.receiveByteArray a 5 mempty
  sample @=? actual

-- This test opens two datagram sockets and send a message from each
-- one to the other. Then it checks that epoll's event-triggered
-- interface correctly notifies the user about the read-readiness
-- that has happened.
testLinuxEpollA :: Assertion
testLinuxEpollA = do
  (a, b) <- demand =<< S.uninterruptibleSocketPair S.Unix S.datagram S.defaultProtocol
  epfd <- demand =<< Epoll.uninterruptibleCreate 1
  reg <- PM.newPrimArray 1
  PM.writePrimArray reg 0 $
    Epoll.Event
      { Epoll.events = Epoll.input <> Epoll.edgeTriggered
      , Epoll.payload = a
      }
  demand =<< Epoll.uninterruptibleControlMutablePrimArray epfd Epoll.add a reg
  PM.writePrimArray reg 0 $
    Epoll.Event
      { Epoll.events = Epoll.input <> Epoll.edgeTriggered
      , Epoll.payload = b
      }
  demand =<< Epoll.uninterruptibleControlMutablePrimArray epfd Epoll.add b reg
  threadWaitWrite b
  bytesSentB <- demand =<< S.uninterruptibleSendByteArray b sample 0 5 mempty
  when (bytesSentB /= 5) (fail "testLinuxEpollA: bytesSentB was wrong")
  threadWaitWrite a
  bytesSentA <- demand =<< S.uninterruptibleSendByteArray a sample 0 5 mempty
  when (bytesSentA /= 5) (fail "testLinuxEpollA: bytesSentA was wrong")
  evs <- PM.newPrimArray 3
  loadGarbage evs
  evCount <- demand =<< Epoll.waitMutablePrimArray epfd evs 3 (-1)
  when (evCount /= 2) (fail ("testLinuxEpollA: evCount was " ++ show evCount))
  r <- case () of
    _ -> do
      Epoll.Event {Epoll.events, Epoll.payload} <- PM.readPrimArray evs 0
      when (payload /= a && payload /= b) (fail ("testLinuxEpollA: payload x was " ++ show payload))
      let Epoll.Events e = events
      when (not (Epoll.containsAnyEvents events Epoll.input)) $ do
        fail ("testLinuxEpollA: events x bitmask " ++ showIntAtBase 2 binChar e " missing EPOLLIN")
      pure payload
  Epoll.Event {Epoll.events, Epoll.payload} <- PM.readPrimArray evs 1
  when (payload == r) (fail ("testLinuxEpollA: same payload " ++ show payload ++ " for both events"))
  when (payload /= a && payload /= b) (fail ("testLinuxEpollA: payload y was " ++ show payload))
  let Epoll.Events e = events
  when (not (Epoll.containsAnyEvents events Epoll.input)) $ do
    fail ("testLinuxEpollA: events y bitmask " ++ showIntAtBase 2 binChar e " missing EPOLLIN")
  pure ()

binChar :: Int -> Char
binChar = \case
  0 -> '0'
  1 -> '1'
  _ -> 'x'

loadGarbage :: MutablePrimArray RealWorld a -> IO ()
loadGarbage (MutablePrimArray x) = do
  let arr = MutableByteArray x
      go :: Int -> IO ()
      go !ix =
        if ix > (-1)
          then do
            PM.writeByteArray arr ix ((0b01010101 :: Word8) + fromIntegral ix)
            go (ix - 1)
          else pure ()
  n <- PM.getSizeofMutableByteArray arr
  go (n - 1)

sample :: ByteArray
sample = E.fromList [1, 2, 3, 4, 5]

demand :: Either Errno a -> IO a
demand = either (\e -> ioError (errnoToIOError "test" e Nothing Nothing)) pure

oneWord :: CSize -> IO ()
oneWord x = if x == fromIntegral (PM.sizeOf (undefined :: Int)) then pure () else fail "expected one machine word"
