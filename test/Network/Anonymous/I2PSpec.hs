{-# LANGUAGE OverloadedStrings #-}

module Network.Anonymous.I2PSpec where

import           Control.Concurrent.MVar
import           Control.Concurrent                      (ThreadId, forkIO,
                                                          killThread,
                                                          threadDelay)
import qualified Network.Socket.ByteString as Network

import           Network.Anonymous.I2P
import           Test.Hspec

spec :: Spec
spec = do
  describe "when serving VirtualStream connections" $ do
    it "source destination should match accepted destination" $
      let storeDestination pubDestValidate' (_, dest) =
            putMVar pubDestValidate' dest

      in do
        (privDest0, pubDest0) <- createDestination Nothing
        (privDest1, pubDest1) <- createDestination Nothing

        pubDestValidate0' <- newEmptyMVar
        pubDestValidate1' <- newEmptyMVar

        threadId <- forkIO $ serveStream   privDest0          (storeDestination pubDestValidate1')

        threadDelay 30000000
        connectStream' privDest1 pubDest0 (storeDestination pubDestValidate0')

        pubDestValidate0 <- takeMVar pubDestValidate0'
        pubDestValidate1 <- takeMVar pubDestValidate1'

        pubDestValidate0 `shouldBe` pubDest0
        pubDestValidate1 `shouldBe` pubDest1

        killThread threadId

    it "sockets should be able to communicate" $
      let sendResponse (sock, _) = do
            putStrLn "Now sending data.."
            Network.sendAll sock "Hello, world!"
            putStrLn "Sent data!"

          readResponse response' (sock, _) = do
            putStrLn "Now calling recv()"
            buf <- Network.recv sock 64
            putStrLn ("Received: " ++ show buf)
            putMVar response' buf

      in do
        (privDest0, pubDest0) <- createDestination Nothing

        response' <- newEmptyMVar

        threadId <- forkIO $ serveStream privDest0 sendResponse

        threadDelay 30000000
        connectStream pubDest0 (readResponse response')

        response <- takeMVar response'
        response `shouldBe` "Hello, world!"

        killThread threadId
