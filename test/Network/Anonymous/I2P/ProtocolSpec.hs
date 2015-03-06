{-# LANGUAGE OverloadedStrings #-}

module Network.Anonymous.I2P.ProtocolSpec where

import           Control.Concurrent                      (ThreadId, forkIO,
                                                          killThread,
                                                          threadDelay)
import           Control.Monad.Catch
import           Control.Monad.IO.Class

import qualified Network.Simple.TCP                      as NS (accept, listen,
                                                                send)
import qualified Network.Socket                          as NS (Socket)

import qualified Network.Anonymous.I2P.Error             as E
import qualified Network.Anonymous.I2P.Protocol as P     (connect,
                                                          createSession,
                                                          createSessionWith,
                                                          version,
                                                          versionWithConstraint,
                                                          acceptStream,
                                                          connectStream)
import qualified Network.Anonymous.I2P.Types.Destination as D
import qualified Network.Anonymous.I2P.Types.Socket      as S
import qualified Network.Anonymous.I2P.Util              as U

import qualified Data.ByteString                         as BS
import           Data.Maybe                              (fromJust, isJust)
import qualified Data.UUID                               as Uuid
import qualified Data.UUID.Util                          as Uuid

import           Test.Hspec

mockServer :: ( MonadIO m
              , MonadMask m)
           => String
           -> (NS.Socket -> IO a)
           -> m ThreadId
mockServer port callback = do
  tid <- liftIO $ forkIO $
    NS.listen "*" port (\(lsock, _) -> NS.accept lsock (\pair -> do
                                                           _ <- callback (fst pair)
                                                           threadDelay 1000000
                                                           return ()))

  liftIO $ threadDelay 500000
  return tid

spec :: Spec
spec = do
  describe "when connecting to a SAM bridge" $ do
    it "connecting to the default port should work" $ do
      P.connect "127.0.0.1" "7656" (\_ -> return ()) `shouldReturn` ()

    it "connecting to a non-existent port should not work" $ do
      (P.connect "127.0.0.1" "1234" return) `shouldThrow` anyIOException

  describe "when negotiating protocol version" $ do
    it "should use protocol version 3.1 when connecting to SAM" $
      P.connect "127.0.0.1" "7656" P.version `shouldReturn` [3,1]

    it "should return an arbitrary version if we supply it" $
      let serverSock = flip NS.send "HELLO REPLY RESULT=OK VERSION=1.2\n"

      in do
        thread <- liftIO $ mockServer "4322" serverSock
        P.connect "127.0.0.1" "4322" P.version `shouldReturn` [1,2]
        killThread thread

    it "should use throw an error if no correct version can be found" $
      P.connect "127.0.0.1" "7656" (P.versionWithConstraint ([4,0], [4,1])) `shouldThrow` U.isI2PError E.noVersionErrorType

    it "should throw an error when the host sends an incomplete reply" $
      let serverSock = flip NS.send "HELLO VERSION REPLY "

      in do
        thread <- liftIO $ mockServer "4323" serverSock
        P.connect "127.0.0.1" "4323" P.version `shouldThrow` anyIOException -- thrown by parser, not our own library
        killThread thread

    it "should throw an error when the host responds with an error" $
      let serverSock = flip NS.send "HELLO REPLY RESULT=I2P_ERROR MESSAGE=\"fooMessage\"\n"

      in do
        thread <- liftIO $ mockServer "4324" serverSock
        P.connect "127.0.0.1" "4324" P.version `shouldThrow` U.isI2PError E.protocolErrorType
        killThread thread

  describe "when creating session" $ do
    it "should be able to create all types of socket types" $
      let createSession socketType pair = P.version pair >> P.createSession socketType pair

          performTest socketType = do
            (sessionId, destination) <- P.connect "127.0.0.1" "7656" (createSession socketType)

            (Uuid.fromString sessionId) `shouldSatisfy` isJust
            (Uuid.version (fromJust (Uuid.fromString sessionId))) `shouldBe` 4
            (BS.length (D.base64 destination)) `shouldSatisfy` (>= 387)

          socketTypes = [ S.VirtualStream
                        , S.DatagramRepliable
                        , S.DatagramAnonymous ]

      in  mapM performTest socketTypes >> return ()

    it "should be able to create new destinations with all signature types" $
      let createSession signatureType pair = P.version pair >> P.createSessionWith Nothing Nothing (Just signatureType) S.VirtualStream pair

          performTest signatureType = P.connect "127.0.0.1" "7656" (createSession signatureType)

          sigTypes =  [ D.DsaSha1
                      , D.EcdsaSha256P256
                      , D.EcdsaSha384P384
                      , D.EcdsaSha512P521
                      , D.RsaSha2562048
                      , D.RsaSha3843072
                      , D.RsaSha5124096
                      , D.EdDsaSha512Ed25519 ]


      -- If something fails here, an exception will be thrown
      in mapM performTest sigTypes >> return ()

    it "should throw a protocol error when creating a session twice" $
      let createSession     socketType pair           = P.createSession socketType pair

          performTest socketType pair = do
            -- Note that we have to be inside the same connection here.
            _ <- P.version pair
            _ <- createSession socketType pair
            createSession socketType pair `shouldThrow` U.isI2PError E.protocolErrorType

      in P.connect "127.0.0.1" "7656" (performTest S.VirtualStream)

    it "should throw a protocol error when creating a session with a duplicated nickname id" $
      let socketType = S.VirtualStream
          phase1 pair1 = do
            _ <- P.version pair1
            (sessionId1, _) <- P.createSession socketType pair1

            P.connect "127.0.0.1" "7656" (phase2 sessionId1)

          phase2 sessionId1 pair2 = do
            _ <- P.version pair2
            P.createSessionWith (Just sessionId1) Nothing Nothing socketType pair2 `shouldThrow` U.isI2PError E.duplicatedSessionIdErrorType

      in P.connect "127.0.0.1" "7656" phase1

    it "should throw an error when providing an invalid destination key" $
      let createSessionWith destinationId socketType pair = P.createSessionWith Nothing destinationId Nothing socketType pair

          performTest socketType pair = do
            _ <- P.version pair
            createSessionWith (Just (D.Destination "123invalid")) socketType pair `shouldThrow` U.isI2PError E.invalidKeyErrorType

      in P.connect "127.0.0.1" "7656" (performTest S.VirtualStream)

    it "should throw a protocol error when creating a session with a duplicated destination key" $
      let socketType = S.VirtualStream
          phase1 pair1 = do
            _ <- P.version pair1
            (_, destination1) <- P.createSession socketType pair1

            P.connect "127.0.0.1" "7656" (phase2 destination1)

          phase2 destination1 pair2 = do
            _ <- P.version pair2
            P.createSessionWith Nothing (Just destination1) Nothing socketType pair2 `shouldThrow` U.isI2PError E.duplicatedDestinationErrorType

      in P.connect "127.0.0.1" "7656" phase1

  describe "when accepting a stream connection" $ do
    it "should be returning an error when we try to accept before creating a session" $
      let phase1 pair = P.version pair >> P.acceptStream "nonExistingSessionId" pair

      in P.connect "127.0.0.1" "7656" phase1 `shouldThrow` U.isI2PError E.invalidIdErrorType

    it "should be returning an error when we try to accept a stream using an invalid sockettype" $
      let socketTypes = [ S.DatagramRepliable
                        , S.DatagramAnonymous ]

          phase2 sessionId pair = P.version pair >> P.acceptStream sessionId pair

          phase1 socketType pair = do
            (sessionId, _) <- P.version pair >> P.createSession socketType pair

            P.connect "127.0.0.1" "7656" (phase2 sessionId)

          performTest socketType = P.connect "127.0.0.1" "7656" (phase1 socketType) `shouldThrow` U.isI2PError E.protocolErrorType

      in mapM performTest socketTypes >> return ()

  describe "when connecting to a stream connection" $ do
    it "should be returning an error when we try to connect before creating a session" $
      let phase1 pair = P.version pair >> P.connectStream "nonExistingSessionId" (D.Destination "123") pair

      in P.connect "127.0.0.1" "7656" phase1 `shouldThrow` U.isI2PError E.invalidIdErrorType

    it "should be returning an error when we try to connect with a stream using an invalid sockettype" $
      let socketTypes = [ S.DatagramRepliable
                        , S.DatagramAnonymous ]

          phase2 sessionId pair = P.version pair >> P.connectStream sessionId (D.Destination "123") pair

          phase1 socketType pair = do
            (sessionId, _) <- P.version pair >> P.createSession socketType pair

            P.connect "127.0.0.1" "7656" (phase2 sessionId)

          performTest socketType = P.connect "127.0.0.1" "7656" (phase1 socketType) `shouldThrow` U.isI2PError E.protocolErrorType

      in mapM performTest socketTypes >> return ()

    it "should be returning an error when we try to connect with a stream to a destination that does not exist" $
      let socketType = S.VirtualStream

          createDestination = do
            P.connect "127.0.0.1" "7656" (\pair -> do
                                             _ <- P.version pair
                                             (_, destination) <- P.createSession socketType pair
                                             return (destination))

          phase2 dest sessionId pair = P.version pair >> P.connectStream sessionId dest pair

          phase1 dest pair = do
            (sessionId, _) <- P.version pair >> P.createSession socketType pair

            P.connect "127.0.0.1" "7656" (phase2 dest sessionId)

      in do
        destination <- createDestination

        -- At this point, the socket is closed and the destination does not any longer exist
        P.connect "127.0.0.1" "7656" (phase1 destination) `shouldThrow` U.isI2PError E.unreachableErrorType
