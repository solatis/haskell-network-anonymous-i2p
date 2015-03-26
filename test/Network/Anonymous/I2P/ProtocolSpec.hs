{-# LANGUAGE OverloadedStrings #-}

module Network.Anonymous.I2P.ProtocolSpec where

import Control.Concurrent.MVar
import           Control.Concurrent                      (ThreadId, forkIO,
                                                          killThread,
                                                          threadDelay)
import           Control.Monad.Catch
import           Control.Monad.IO.Class

import qualified Network.Simple.TCP                      as NS (accept, listen,
                                                                send)
import qualified Network.Socket                          as NS (Socket)
import qualified Network.Socket.ByteString               as NSB (sendAll, recv)

import qualified Network.Anonymous.I2P.Error             as E
import qualified Network.Anonymous.I2P.Protocol as P     (connect,
                                                          createDestination,
                                                          createSession,
                                                          createSessionWith,
                                                          version,
                                                          versionWithConstraint,
                                                          acceptStream,
                                                          connectStream,
                                                          sendDatagram,
                                                          receiveDatagram)
import qualified Network.Anonymous.I2P.Types.Destination as D
import qualified Network.Anonymous.I2P.Types.Socket      as S
import qualified Network.Anonymous.I2P.Util              as U

import qualified Data.ByteString                         as BS
import qualified Data.ByteString.Char8                   as BS8
import           Data.Maybe                              (fromJust, isJust, isNothing)
import qualified Data.UUID                               as Uuid
import qualified Data.UUID.Util                          as Uuid
import qualified Data.UUID.V4                            as Uuid

import           Test.Hspec

mockServer :: ( MonadIO m
              , MonadMask m)
           => String
           -> (NS.Socket -> IO a)
           -> m ThreadId
mockServer port callback = do
  tid <- liftIO $ forkIO $
    NS.listen "*" port (\(lsock, _) -> NS.accept lsock (\(sock, _) -> do
                                                           _ <- callback sock
                                                           threadDelay 1000000
                                                           return ()))

  liftIO $ threadDelay 500000
  return tid

testSockets :: NS.Socket -> NS.Socket -> IO (BS.ByteString, BS.ByteString)
testSockets sink source =
  let uuidAsBs           = BS8.pack . Uuid.toString

      recvAll :: NS.Socket -> Int -> IO BS.ByteString
      recvAll _ 0 = return (BS.empty)
      recvAll sock bytes = do
        recv  <- NSB.recv sock bytes
        recv' <- recvAll sock (bytes - BS.length recv)

        return (BS.append recv recv')

  in do
    uuid <- Uuid.nextRandom

    NSB.sendAll sink (uuidAsBs uuid)

    received <- recvAll source (BS.length (uuidAsBs uuid))
    return (uuidAsBs uuid, received)

spec :: Spec
spec = do
  describe "when connecting to a SAM bridge" $ do
    it "connecting to the default port should work" $ do
      P.connect "127.0.0.1" "7656" (\_ -> return ()) `shouldReturn` ()

    it "connecting to a non-existent port should not work" $ do
      (P.connect "127.0.0.1" "1234" return) `shouldThrow` anyIOException

  describe "when negotiating protocol version" $ do
    it "should use protocol version 3.1 when connecting to SAM" $
      P.connect "127.0.0.1" "7656" (P.version . fst) `shouldReturn` [3,1]

    it "should return an arbitrary version if we supply it" $
      let serverSock = flip NS.send "HELLO REPLY RESULT=OK VERSION=1.2\n"

      in do
        thread <- liftIO $ mockServer "4322" serverSock
        P.connect "127.0.0.1" "4322" (P.version . fst) `shouldReturn` [1,2]
        killThread thread

    it "should use throw an error if no correct version can be found" $
      P.connect "127.0.0.1" "7656" (P.versionWithConstraint ([4,0], [4,1]) . fst) `shouldThrow` U.isI2PError E.noVersionErrorType

    it "should throw an error when the host sends an incomplete reply" $
      let serverSock = flip NS.send "HELLO VERSION REPLY "

      in do
        thread <- liftIO $ mockServer "4323" serverSock
        P.connect "127.0.0.1" "4323" (P.version . fst) `shouldThrow` anyIOException -- thrown by parser, not our own library
        killThread thread

    it "should throw an error when the host responds with an error" $
      let serverSock = flip NS.send "HELLO REPLY RESULT=I2P_ERROR MESSAGE=\"fooMessage\"\n"

      in do
        thread <- liftIO $ mockServer "4324" serverSock
        P.connect "127.0.0.1" "4324" (P.version . fst) `shouldThrow` U.isI2PError E.protocolErrorType
        killThread thread

  describe "when creating a destination" $ do
    it "should be able to create new destinations with all signature types" $
      let createSession signatureType (sock, _) = do
            (priv, pub) <- P.version sock >> P.createDestination (Just signatureType) sock
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
      let createSession socketType (sock, _) = do
            (privDestination, _) <- P.version sock >> P.createDestination Nothing sock
            P.createSessionWith Nothing privDestination socketType sock

          performTest socketType = do
            sessionId <- P.connect "127.0.0.1" "7656" (createSession socketType)

            (Uuid.fromString sessionId) `shouldSatisfy` isJust
            (Uuid.version (fromJust (Uuid.fromString sessionId))) `shouldBe` 4

          socketTypes = [ S.VirtualStream
                        , S.DatagramRepliable
                        , S.DatagramAnonymous ]

      in  mapM performTest socketTypes >> return ()

    it "should throw a protocol error when creating a session twice" $
      let performTest socketType (sock, _) = do
            -- Note that we have to be inside the same connection here.
            _ <- P.version sock
            _ <- P.createSession socketType sock
            P.createSession socketType sock `shouldThrow` U.isI2PError E.protocolErrorType

      in P.connect "127.0.0.1" "7656" (performTest S.VirtualStream)

    it "should throw a protocol error when creating a session with a duplicated nickname id" $
      let socketType   = S.VirtualStream
          phase1 (sock1, _) = do
            (privDestination1, _) <- P.version sock1 >> P.createDestination Nothing sock1
            sessionId1            <- P.createSessionWith Nothing privDestination1 socketType sock1

            P.connect "127.0.0.1" "7656" (phase2 sessionId1)

          phase2 sessionId1 (sock2, _) = do
            (privDestination2, _) <- P.version sock2 >> P.createDestination Nothing sock2
            P.createSessionWith (Just sessionId1) privDestination2 socketType sock2 `shouldThrow` U.isI2PError E.duplicatedSessionIdErrorType

      in P.connect "127.0.0.1" "7656" phase1

    it "should throw an error when providing an invalid destination key" $
      let performTest socketType (sock, _) = do
            _ <- P.version sock
            P.createSessionWith Nothing (D.PrivateDestination "123invalid") socketType sock `shouldThrow` U.isI2PError E.invalidKeyErrorType

      in P.connect "127.0.0.1" "7656" (performTest S.VirtualStream)

    it "should throw a protocol error when creating a session with a duplicated destination key" $
      let socketType = S.VirtualStream

          phase2 privDestination1 (sock2, _) = do
            (P.version sock2 >> P.createSessionWith Nothing privDestination1 socketType sock2) `shouldThrow` U.isI2PError E.duplicatedDestinationErrorType

          phase1 (sock1, _) = do
            (privDestination1, _) <- P.version sock1 >> P.createDestination Nothing sock1
            _                     <- P.createSessionWith Nothing privDestination1 socketType sock1

            P.connect "127.0.0.1" "7656" (phase2 privDestination1)

      in P.connect "127.0.0.1" "7656" phase1

  describe "when accepting a stream connection" $ do
    it "should be returning an error when we try to accept before creating a session" $
      let phase1 (sock, _) = P.version sock >> P.acceptStream "nonExistingSessionId" sock

      in P.connect "127.0.0.1" "7656" phase1 `shouldThrow` U.isI2PError E.invalidIdErrorType

    it "should be returning an error when we try to accept a stream using an invalid sockettype" $
      let socketTypes = [ S.DatagramRepliable
                        , S.DatagramAnonymous ]

          phase2 sessionId (sock, _) = P.version sock >> P.acceptStream sessionId sock

          phase1 socketType (sock, _) = do
            (privDestination, _) <- P.version sock >> P.createDestination Nothing sock
            sessionId <- P.createSessionWith Nothing privDestination socketType sock

            P.connect "127.0.0.1" "7656" (phase2 sessionId)

          performTest socketType = P.connect "127.0.0.1" "7656" (phase1 socketType) `shouldThrow` U.isI2PError E.protocolErrorType

      in mapM performTest socketTypes >> return ()

  describe "when connecting to a stream connection" $ do
    it "should be returning an error when we try to connect before creating a session" $
      let phase1 (sock, _) = P.version sock >> P.connectStream "nonExistingSessionId" (D.PublicDestination "123") sock

      in P.connect "127.0.0.1" "7656" phase1 `shouldThrow` U.isI2PError E.invalidIdErrorType

    it "should be returning an error when we try to connect with a stream using an invalid sockettype" $
      let socketTypes = [ S.DatagramRepliable
                        , S.DatagramAnonymous ]

          phase2 sessionId (sock, _) = P.version sock >> P.connectStream sessionId (D.PublicDestination "123") sock

          phase1 socketType (sock, _) = do
            (privDestination, _) <- P.version sock >> P.createDestination Nothing sock
            sessionId <- P.createSessionWith Nothing privDestination socketType sock

            P.connect "127.0.0.1" "7656" (phase2 sessionId)

          performTest socketType = P.connect "127.0.0.1" "7656" (phase1 socketType) `shouldThrow` U.isI2PError E.protocolErrorType

      in mapM performTest socketTypes >> return ()

    it "should be returning an error when we try to connect with a stream with an invalid destination type" $
      let socketType = S.VirtualStream

          phase2 sessionId (sock, _) = P.version sock >> P.connectStream sessionId (D.PublicDestination "invalidDestination") sock

          phase1 (sock, _) = do
            (privDestination, _) <- P.version sock >> P.createDestination Nothing sock
            sessionId <- P.createSessionWith Nothing privDestination socketType sock

            P.connect "127.0.0.1" "7656" (phase2 sessionId)

      in P.connect "127.0.0.1" "7656" phase1 `shouldThrow` U.isI2PError E.invalidKeyErrorType

    it "should be returning an error when we try to connect with a stream to a destination that does not exist" $
      let socketType = S.VirtualStream

          createDestination = do
            P.connect "127.0.0.1" "7656" (\(sock, _) -> do
                                             (_, destination) <- P.version sock >> P.createDestination Nothing sock
                                             return (destination))

          phase2 dest sessionId (sock, _) = P.version sock >> P.connectStream sessionId dest sock

          phase1 dest (sock, _) = do
            (privateDestination, _) <- P.version sock >> P.createDestination Nothing sock
            sessionId               <- P.createSessionWith Nothing privateDestination socketType sock

            P.connect "127.0.0.1" "7656" (phase2 dest sessionId)

      in do
        destination <- createDestination

        -- At this point, the socket is closed and the destination does not any longer exist
        P.connect "127.0.0.1" "7656" (phase1 destination) `shouldThrow` U.isI2PError E.unreachableErrorType

    it "should be returning an unreachable error when we do not accept the connection" $
      let connectConnection destination sessionId (sock, _) =
            P.version sock >> P.connectStream sessionId destination sock

          phase2 publicDestinationAccept (sockConnect, _) = do
            putStrLn "creating connect session"
            (privateDestinationConnect, _) <- P.version sockConnect >> P.createDestination Nothing sockConnect
            sessionIdConnect               <- P.createSessionWith Nothing privateDestinationConnect S.VirtualStream sockConnect

            putStrLn "start connecting to accept destination"

            -- At this point, we should be able to connect
            P.connect "127.0.0.1" "7656" (connectConnection publicDestinationAccept sessionIdConnect) `shouldThrow` U.isI2PError E.unreachableErrorType

          phase1 (sockAccept, _) = do
            putStrLn "creating accept session"
            (privateDestinationAccept, publicDestinationAccept) <- P.version sockAccept >> P.createDestination Nothing sockAccept

            _ <- P.createSessionWith Nothing privateDestinationAccept S.VirtualStream sockAccept
            P.connect "127.0.0.1" "7656" (phase2 publicDestinationAccept)

      in P.connect "127.0.0.1" "7656" phase1


  describe "when accepting and connecting to a stream connection" $ do
    it "accepted connection and originating connection should be able to communicate" $
      let socketType = S.VirtualStream

          acceptAndTestConnection sessionId connectDestination connectSockMVar finishedTest (sock, _) = do
            (acceptSock, acceptDestination) <- P.version sock >> P.acceptStream sessionId sock

            -- The destination we just accepted should equal the destination we
            -- connected from.
            connectDestination `shouldBe` acceptDestination

            -- Now, let's grab the socket we used to connect and validate we can
            -- send messages in both directions.
            connectSock <- takeMVar connectSockMVar

            (sent1, recv1) <- testSockets acceptSock connectSock
            (sent2, recv2) <- testSockets connectSock acceptSock

            sent1 `shouldBe` recv1
            sent2 `shouldBe` recv2

            putMVar finishedTest True
            return ()

          connectConnection destination sessionId connectSockMVar finishedTestMVar (sock, _) = do
            P.version sock >> P.connectStream sessionId destination sock

            putMVar connectSockMVar sock

            -- This blocks until the test case is finished, to avoid closing sockets.
            finished <- takeMVar finishedTestMVar
            finished `shouldBe` True

          phase2 sessionIdAccept publicDestinationAccept (sockConnect, _) = do
            (privateDestinationConnect, publicDestinationConnect) <- P.version sockConnect >> P.createDestination Nothing sockConnect
            sessionIdConnect                                      <- P.createSessionWith Nothing privateDestinationConnect socketType sockConnect

            finishedTestMVar <- newEmptyMVar
            connectSockMVar  <- newEmptyMVar

            -- Start accepting connections, and wait for a second to get our
            -- 'accept' to come alive.
            threadId <- forkIO $ P.connect "127.0.0.1" "7656" (acceptAndTestConnection sessionIdAccept publicDestinationConnect connectSockMVar finishedTestMVar)
            threadDelay 1000000

            -- At this point, we should be able to connect
            _ <- P.connect "127.0.0.1" "7656" (connectConnection publicDestinationAccept sessionIdConnect connectSockMVar finishedTestMVar)

            killThread threadId

          phase1 (sockAccept, _) = do
            putStrLn "creating accept session"
            (privateDestinationAccept, publicDestinationAccept) <- P.version sockAccept >> P.createDestination Nothing sockAccept

            sessionIdAccept <- P.createSessionWith Nothing privateDestinationAccept socketType sockAccept
            P.connect "127.0.0.1" "7656" (phase2 sessionIdAccept publicDestinationAccept)

      in P.connect "127.0.0.1" "7656" phase1




  describe "when sending and receiving datagram messages" $ do
    it "should be able to receive a datagram message with a reply destination" $
      let socketTypes = [ S.DatagramRepliable
                        , S.DatagramAnonymous ]

          createSink socketType sinkSock' sinkDestination' testFinished' (sockSink, _) = do
            putStrLn "creating sink session"
            (privateDestinationSink, publicDestinationSink) <- P.version sockSink >> P.createDestination Nothing sockSink
            _ <- P.createSessionWith Nothing privateDestinationSink socketType sockSink

            putMVar sinkDestination' publicDestinationSink
            putMVar sinkSock' sockSink

            -- Block until test is finished, so the sockets stay alive
            finished <- readMVar testFinished'
            finished `shouldBe` True

          createSource socketType sourceSession testFinished (sockSource, _) = do
            putStrLn "creating source session"
            (privateDestinationSource, _) <- P.version sockSource >> P.createDestination Nothing sockSource
            sessionIdSource <- P.createSessionWith Nothing privateDestinationSource socketType sockSource

            putMVar sourceSession sessionIdSource

            -- Block until test is finished, so the sockets stay alive
            finished <- readMVar testFinished
            finished `shouldBe` True

          performTest socketType = do
            testFinished'    <- newEmptyMVar

            -- Our 'source' socket is the party that sends the datagram
            sourceSessionId' <- newEmptyMVar

            -- Our 'sink' socket is the party that receives the datagram
            sinkSock'        <- newEmptyMVar
            sinkDestination' <- newEmptyMVar

            _ <- forkIO $ P.connect "127.0.0.1" "7656" (createSink   socketType sinkSock' sinkDestination' testFinished')
            _ <- forkIO $ P.connect "127.0.0.1" "7656" (createSource socketType           sourceSessionId' testFinished')

            -- First, we need our source socket and session id, and the destination
            -- where we should send the message to.
            sinkDestination <- takeMVar sinkDestination'
            sourceSessionId <- takeMVar sourceSessionId'

            -- Now, lets' wait until the sink socket is ready to receive
            sinkSock        <- takeMVar sinkSock'

            -- Put a message on top of our source socket
            P.sendDatagram ("127.0.0.1", "7655") sourceSessionId sinkDestination (BS8.pack "Hello, world!")

            putStrLn "Now attempting to receive a datagram in the sink"

            -- Wait for a message to arrive on our sink socket
            (msg, destination) <- P.receiveDatagram sinkSock

            msg `shouldBe` (BS8.pack "Hello, world!")

            case socketType of
             S.DatagramAnonymous -> destination `shouldSatisfy` isNothing
             S.DatagramRepliable -> destination `shouldSatisfy` isJust
             _                   -> fail ("internal error: unrecognized socketType")

            return ()

      in mapM performTest socketTypes >> return ()


    it "datagram messages have size limitations" $
      let bigMessage =
           BS.concat (replicate 3174 "lorem ipsum hello world foo bar baz")

          createSink sinkDestination (sockSink, _) = do
            (publicDestinationSink, _) <- P.version sockSink >> P.createDestination Nothing sockSink
            putMVar sinkDestination publicDestinationSink

          createSource sourceSession (sockSource, _) = do
            (privateDestinationSource, _) <- P.version sockSource >> P.createDestination Nothing sockSource
            sessionIdSource <- P.createSessionWith Nothing privateDestinationSource S.DatagramRepliable sockSource

            putMVar sourceSession sessionIdSource

          performTest = do
            -- Our 'source' socket is the party that sends the datagram
            sourceSessionId' <- newEmptyMVar

            -- Our 'sink' socket is the party that receives the datagram
            sinkDestination' <- newEmptyMVar

            _ <- forkIO $ P.connect "127.0.0.1" "7656" (createSink   sinkDestination')
            _ <- forkIO $ P.connect "127.0.0.1" "7656" (createSource sourceSessionId')

            -- First, we need our source socket and session id, and the destination
            -- where we should send the message to.
            sinkDestination <- takeMVar sinkDestination'
            sourceSessionId <- takeMVar sourceSessionId'

            -- Put a message on top of our source socket
            P.sendDatagram ("127.0.0.1", "7655") sourceSessionId sinkDestination bigMessage `shouldThrow`  U.isI2PError E.messageTooLongErrorType

            return ()

      in performTest
