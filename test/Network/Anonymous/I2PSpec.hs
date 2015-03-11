{-# LANGUAGE OverloadedStrings #-}

module Network.Anonymous.I2PSpec where

import           Control.Concurrent.MVar
import           Control.Concurrent                      (forkIO,
                                                          threadDelay,
                                                          killThread)
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

        threadId <- forkIO $ withSession' S.VirtualStream (privDest0, pubDest0) (\ctx -> serveStream ctx (storeDestination pubDestValidate1'))

        threadDelay 30000000
        withSession' S.VirtualStream (privDest1, pubDest1) (\ctx -> connectStream ctx pubDest0 (storeDestination pubDestValidate0'))

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

        threadId <- forkIO $ withSession' S.VirtualStream (privDest0, pubDest0) (\ctx -> serveStream ctx sendResponse)

        threadDelay 30000000
        withSession S.VirtualStream (\ctx -> connectStream ctx pubDest0 (readResponse response'))

        response <- takeMVar response'
        response `shouldBe` "Hello, world!"

        killThread threadId

  describe "when serving datagrams connections" $ do
    it "messages are received along with their reply address or not" $
      let socketTypes = [ S.DatagramAnonymous
                        , S.DatagramRepliable]

          receiveMessage msg' pubDest' (msg, pubDest) = do
            putStrLn ("received message: " ++ show msg)
            putMVar msg'     msg
            putMVar pubDest' pubDest
            putStrLn ("stored mvars")

          sendMessage ctx dest msg = do
            threadDelay 30000000
            putStrLn ("sending message: " ++ show msg)
            sendDatagram ctx dest msg

          performTest socketType = do
            (privDest0, pubDest0) <- createDestination Nothing

            msg'          <- newEmptyMVar
            sessionReady' <- newEmptyMVar
            pubDest1'     <- newEmptyMVar
            finished'     <- newEmptyMVar

            threadId1 <- forkIO $ withSession' socketType (privDest0, pubDest0) (\ctx -> do
                                                                                    _ <- putMVar sessionReady' True
                                                                                    serveDatagram ctx (receiveMessage  msg' pubDest1'))

            _         <- takeMVar sessionReady'
            threadId2 <- forkIO $ withSession socketType (\ctx -> do
                                                             sendMessage ctx pubDest0 "Hello, world!\n"

                                                             -- This blocks until the test is finished
                                                             _ <- readMVar finished'
                                                             return ())

            msg      <- readMVar msg'
            pubDest1 <- readMVar pubDest1'

            putMVar finished' True

            putStrLn("got mvars: msg=" ++ show msg ++ ", pubDest1=" ++ show pubDest1)

            msg `shouldBe` "Hello, world!\n"

            case socketType of
             S.DatagramAnonymous -> pubDest1 `shouldSatisfy` isNothing
             S.DatagramRepliable -> pubDest1 `shouldSatisfy` isJust

            return ()

      in mapM performTest socketTypes >> return ()
