{-# LANGUAGE OverloadedStrings #-}

module Network.Anonymous.I2PSpec where

import           Control.Concurrent.MVar
import           Control.Concurrent                      (forkIO,
                                                          threadDelay)
import qualified Network.Socket.ByteString as Network
import           Data.Maybe                              (isJust, isNothing)

import            Network.Anonymous.I2P
import qualified  Network.Anonymous.I2P.Types.Socket as S
import            Test.Hspec

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

  describe "when serving datagrams connections" $ do
    it "messages are received along with their reply address or not" $
      let socketTypes = [ S.DatagramAnonymous
                        , S.DatagramRepliable]

          receiveMessage msg' pubDest' (msg, pubDest) = do
            putStrLn ("received message: " ++ show msg)
            putMVar msg'     msg
            putMVar pubDest' pubDest
            putStrLn ("stored mvars")

          retrySendMessage dest socketType msg = do
            threadDelay 5000000
            sendDatagram dest socketType msg

            -- Enter recursion
            retrySendMessage dest socketType msg

          performTest socketType = do
            (privDest0, pubDest0) <- createDestination Nothing

            msg'      <- newEmptyMVar
            pubDest1' <- newEmptyMVar

            threadId1 <- forkIO $ serveDatagram    privDest0 socketType (receiveMessage msg' pubDest1')
            threadId2 <- forkIO $ retrySendMessage pubDest0  socketType "Hello, world!\n"

            msg      <- takeMVar msg'
            pubDest1 <- takeMVar pubDest1'

            msg `shouldBe` "Hello, world!\n"

            case socketType of
             S.DatagramAnonymous -> pubDest1 `shouldSatisfy` isNothing
             S.DatagramRepliable -> pubDest1 `shouldSatisfy` isJust

            return ()

      in mapM performTest socketTypes >> return ()
