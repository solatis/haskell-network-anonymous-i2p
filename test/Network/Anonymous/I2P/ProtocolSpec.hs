{-# LANGUAGE OverloadedStrings #-}

module Network.Anonymous.I2P.ProtocolSpec where

import           Control.Concurrent                      (ThreadId, forkIO,
                                                          killThread,
                                                          threadDelay)
import Control.Concurrent.MVar
import           Control.Monad.Catch
import           Control.Monad.IO.Class

import qualified Network.Simple.TCP                      as NS (accept, listen,
                                                                send)
import qualified Network.Socket                          as NS (Socket)

import qualified Network.Anonymous.I2P.Error             as E
import qualified Network.Anonymous.I2P.Protocol as P     (connect,
                                                          createDestination,
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

  describe "when creating a destination" $ do
    it "should be able to create new destinations with all signature types" $
      let createSession signatureType pair = do
            (priv, pub) <- P.version pair >> P.createDestination (Just signatureType) pair
            return ((D.asByteString priv, D.asByteString pub))

          performTest signatureType = do
            (priv, pub) <- P.connect "127.0.0.1" "7656" (createSession signatureType)

            -- Validate that our private signature starts with our public sig.
            -- Note that we strip the last 2 bytes, since they might not match
            -- and be just padding bytes.
            (BS.take ((BS.length pub) - 3) priv) `shouldBe` (BS.take ((BS.length pub) - 3) pub)

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

  describe "when creating a session" $ do
    it "should be able to create all types of socket types" $
      let createSession socketType pair = do
            (privDestination, _) <- P.version pair >> P.createDestination Nothing pair
            P.createSessionWith Nothing privDestination socketType pair

          performTest socketType = do
            sessionId <- P.connect "127.0.0.1" "7656" (createSession socketType)

            (Uuid.fromString sessionId) `shouldSatisfy` isJust
            (Uuid.version (fromJust (Uuid.fromString sessionId))) `shouldBe` 4

          socketTypes = [ S.VirtualStream
                        , S.DatagramRepliable
                        , S.DatagramAnonymous ]

      in  mapM performTest socketTypes >> return ()

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
            (privDestination1, _) <- P.version pair1 >> P.createDestination Nothing pair1
            sessionId1            <- P.createSessionWith Nothing privDestination1 socketType pair1

            P.connect "127.0.0.1" "7656" (phase2 sessionId1)

          phase2 sessionId1 pair2 = do
            (privDestination2, _) <- P.version pair2 >> P.createDestination Nothing pair2
            P.createSessionWith (Just sessionId1) privDestination2 socketType pair2 `shouldThrow` U.isI2PError E.duplicatedSessionIdErrorType

      in P.connect "127.0.0.1" "7656" phase1

    it "should throw an error when providing an invalid destination key" $
      let performTest socketType pair = do
            _ <- P.version pair
            P.createSessionWith Nothing (D.PrivateDestination "123invalid") socketType pair `shouldThrow` U.isI2PError E.invalidKeyErrorType

      in P.connect "127.0.0.1" "7656" (performTest S.VirtualStream)

    it "should throw a protocol error when creating a session with a duplicated destination key" $
      let socketType = S.VirtualStream

          phase2 privDestination1 pair2 = do
            (P.version pair2 >> P.createSessionWith Nothing privDestination1 socketType pair2) `shouldThrow` U.isI2PError E.duplicatedDestinationErrorType

          phase1 pair1 = do
            (privDestination1, _) <- P.version pair1 >> P.createDestination Nothing pair1
            _                     <- P.createSessionWith Nothing privDestination1 socketType pair1

            P.connect "127.0.0.1" "7656" (phase2 privDestination1)

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
            (privDestination, _) <- P.version pair >> P.createDestination Nothing pair
            sessionId <- P.createSessionWith Nothing privDestination socketType pair

            P.connect "127.0.0.1" "7656" (phase2 sessionId)

          performTest socketType = P.connect "127.0.0.1" "7656" (phase1 socketType) `shouldThrow` U.isI2PError E.protocolErrorType

      in mapM performTest socketTypes >> return ()

  describe "when connecting to a stream connection" $ do
    it "should be returning an error when we try to connect before creating a session" $
      let phase1 pair = P.version pair >> P.connectStream "nonExistingSessionId" (D.PublicDestination "123") pair

      in P.connect "127.0.0.1" "7656" phase1 `shouldThrow` U.isI2PError E.invalidIdErrorType

    it "should be returning an error when we try to connect with a stream using an invalid sockettype" $
      let socketTypes = [ S.DatagramRepliable
                        , S.DatagramAnonymous ]

          phase2 sessionId pair = P.version pair >> P.connectStream sessionId (D.PublicDestination "123") pair

          phase1 socketType pair = do
            (privDestination, _) <- P.version pair >> P.createDestination Nothing pair
            sessionId <- P.createSessionWith Nothing privDestination socketType pair

            P.connect "127.0.0.1" "7656" (phase2 sessionId)

          performTest socketType = P.connect "127.0.0.1" "7656" (phase1 socketType) `shouldThrow` U.isI2PError E.protocolErrorType

      in mapM performTest socketTypes >> return ()

    it "should be returning an error when we try to connect with a stream with an invalid destination type" $
      let socketType = S.VirtualStream

          phase2 sessionId pair = P.version pair >> P.connectStream sessionId (D.PublicDestination "invalidDestination") pair

          phase1 pair = do
            (privDestination, _) <- P.version pair >> P.createDestination Nothing pair
            sessionId <- P.createSessionWith Nothing privDestination socketType pair

            P.connect "127.0.0.1" "7656" (phase2 sessionId)

      in P.connect "127.0.0.1" "7656" phase1 `shouldThrow` U.isI2PError E.invalidKeyErrorType

    it "should be returning an error when we try to connect with a stream to a destination that does not exist" $
      let socketType = S.VirtualStream

          createDestination = do
            P.connect "127.0.0.1" "7656" (\pair -> do
                                             (_, destination) <- P.version pair >> P.createDestination Nothing pair
                                             return (destination))

          phase2 dest sessionId pair = P.version pair >> P.connectStream sessionId dest pair

          phase1 dest pair = do
            (privateDestination, _) <- P.version pair >> P.createDestination Nothing pair
            sessionId               <- P.createSessionWith Nothing privateDestination socketType pair

            P.connect "127.0.0.1" "7656" (phase2 dest sessionId)

      in do
        destination <- createDestination

        -- At this point, the socket is closed and the destination does not any longer exist
        P.connect "127.0.0.1" "7656" (phase1 destination) `shouldThrow` U.isI2PError E.unreachableErrorType

    it "should be returning an unreachable error when we do not accept the connection" $
      let connectConnection destination sessionId pair =
            P.version pair >> P.connectStream sessionId destination pair

          phase2 publicDestinationAccept pairConnect = do
            putStrLn "creating connect session"
            (privateDestinationConnect, _) <- P.version pairConnect >> P.createDestination Nothing pairConnect
            sessionIdConnect               <- P.createSessionWith Nothing privateDestinationConnect S.VirtualStream pairConnect

            putStrLn "start connecting to accept destination"

            -- At this point, we should be able to connect
            P.connect "127.0.0.1" "7656" (connectConnection publicDestinationAccept sessionIdConnect) `shouldThrow` U.isI2PError E.unreachableErrorType

          phase1 pairAccept = do
            putStrLn "creating accept session"
            (privateDestinationAccept, publicDestinationAccept) <- P.version pairAccept >> P.createDestination Nothing pairAccept

            _ <- P.createSessionWith Nothing privateDestinationAccept S.VirtualStream pairAccept
            P.connect "127.0.0.1" "7656" (phase2 publicDestinationAccept)

      in P.connect "127.0.0.1" "7656" phase1


  describe "when accepting and connecting to a stream connection" $ do
    it "accepted connection destination should equal connecting session destination" $
      let socketType = S.VirtualStream

          acceptConnection sessionId acceptedConnection pair =
            P.version pair >> P.acceptStream sessionId pair >>= putMVar acceptedConnection

          connectConnection destination sessionId pair =
            P.version pair >> P.connectStream sessionId destination pair

          phase2 sessionIdAccept publicDestinationAccept pairConnect = do
            putStrLn "creating connect session"
            (privateDestinationConnect, publicDestinationConnect) <- P.version pairConnect >> P.createDestination Nothing pairConnect
            sessionIdConnect                                      <- P.createSessionWith Nothing privateDestinationConnect socketType pairConnect

            putStrLn "start accepting connections"

            -- Start accepting connections, and wait for a second to get our
            -- 'accept' to come alive.
            acceptedConnection <- newEmptyMVar
            threadId <- forkIO $ P.connect "127.0.0.1" "7656" (acceptConnection sessionIdAccept acceptedConnection)
            threadDelay 1000000

            putStrLn "start connecting to accept destination"

            -- At this point, we should be able to connect
            _ <- P.connect "127.0.0.1" "7656" (connectConnection publicDestinationAccept sessionIdConnect)
            acceptedPair <- readMVar acceptedConnection

            snd (acceptedPair) `shouldBe` publicDestinationConnect
            killThread threadId

          phase1 pairAccept = do
            putStrLn "creating accept session"
            (privateDestinationAccept, publicDestinationAccept) <- P.version pairAccept >> P.createDestination Nothing pairAccept

            sessionIdAccept <- P.createSessionWith Nothing privateDestinationAccept socketType pairAccept
            P.connect "127.0.0.1" "7656" (phase2 sessionIdAccept publicDestinationAccept)

      in P.connect "127.0.0.1" "7656" phase1
