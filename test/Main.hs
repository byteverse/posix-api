import Test.Tasty
import Test.Tasty.HUnit
import Foreign.C.Error (Errno,errnoToIOError)
import Data.Primitive (ByteArray)
import Control.Concurrent (forkIO)
import Control.Monad (when)

import qualified GHC.Exts as E
import qualified Data.Primitive.MVar as PM
import qualified Posix.Socket as S
import qualified Posix.Socket.Types as P

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
  [ testGroup "sockets"
    [ testCase "pair" testPair
    ]
  ]

testPair :: Assertion
testPair = do
  (a,b) <- demand (S.socketPair P.unix P.datagram P.defaultProtocol)
  m <- PM.newEmptyMVar
  _ <- forkIO $ S.receiveByteArray b 5 mempty >>= PM.putMVar m
  bytesSent <- demand (S.sendByteArray a sample 5 mempty)
  when (bytesSent /= 5) (fail "testPair: bytesSent was wrong")
  actual <- demand (PM.takeMVar m)
  actual @=? sample

sample :: ByteArray
sample = E.fromList [1,2,3,4,5]

demand :: IO (Either Errno a) -> IO a
demand a = a >>= either (\e -> ioError (errnoToIOError "test" e Nothing Nothing)) pure
  

