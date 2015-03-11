{-# LANGUAGE FlexibleContexts #-}

-- | Main interface to I2P
module Network.Anonymous.I2P ( createDestination
                             , withSession
                             , withSession'
                             , connectStream
                             , serveDatagram
                             , serveStream
                             , sendDatagram) where

import           Control.Concurrent                      (forkIO)
import           Control.Concurrent.MVar
import           Control.Monad                           (forever)
import           Control.Monad.Catch
import           Control.Monad.IO.Class

import qualified Data.ByteString                         as BS
import qualified Network.Socket                          as Network

import qualified Network.Anonymous.I2P.Protocol          as P
import qualified Network.Anonymous.I2P.Types.Destination as D
import qualified Network.Anonymous.I2P.Types.Session     as S
import qualified Network.Anonymous.I2P.Types.Socket      as S

-- | Create a new I2P destination endpoint.
--
--   All communication in I2P starts with having our own host endpoint
--   other people can use to communicate with us. This destination consists
--   of a public and a private part: the 'D.PrivateDestination' you can use
--   to accept connections / messages from other people, the 'D.PublicDestination'
--   you can give out to other people to send messages to you.
--
--   __Warning__: Never give out your 'D.PrivateDestination' to other people. It
--                contains your private key, and could be used by other people
--                to effectively MITM you. Use your 'D.PublicDestination' to announce
--                the address other people can connect to.
createDestination :: ( MonadIO m
                     , MonadMask m)
                  => Maybe D.SignatureType                         -- ^ Algorithm to use for signature encryption. As per I2P spec defaults to DSA_SHA1.
                  -> m (D.PrivateDestination, D.PublicDestination) -- ^ The private and public destinations.
createDestination signatureType =
  P.connect "127.0.0.1" "7656" (\(sock, _) -> do
                                   _ <- P.version sock
                                   P.createDestination signatureType sock)


-- | Starts a new I2P session. A connection with the SAM bridge will be
--   established, and a 'S.Context' object will be created and passed to the
--   callback. This context object is then required for all further operations.
--
--   After the callback computation finishes, all acquired resources will be
--   properly closed.
withSession :: ( MonadIO m
               , MonadMask m)
            => S.SocketType       -- ^ The type of socket we will be using.
            -> (S.Context -> m a) -- ^ The computation to run
            -> m a
withSession socketType callback = do
  destPair <- createDestination Nothing
  withSession' socketType destPair callback

-- | Alternative implementation of 'withSession' that explicitly accepts a
--   'D.Destination' pair to use to set up the session. This can be useful
--   if you want to use a specific 'D.SignatureType' to create a local
--   endpoint.
withSession' :: ( MonadIO m
                , MonadMask m)
             => S.SocketType                                -- ^ The type of socket we will be using.
             -> (D.PrivateDestination, D.PublicDestination) -- ^ Destination to use
             -> (S.Context -> m a)                          -- ^ The computation to run
             -> m a
withSession' socketType (privDest, pubDest) callback =
  let bindSession (sock, _) = do
        _         <- P.version sock
        sessionId <- P.createSessionWith Nothing privDest socketType sock

        callback (S.Context sock socketType sessionId privDest pubDest)

  in P.connect "127.0.0.1" "7656" bindSession

-- | Starts a server to accept 'S.VirtualStream' connections from other hosts
--   and handles them concurrently in different threads. Any acquired resources
--   are cleaned up when the computation ends.
serveStream :: ( MonadIO m
               , MonadMask m)
            => S.Context
            -> ((Network.Socket, D.PublicDestination) -> IO ())
            -> m ()
serveStream (S.Context _ _ sessionId _ _) callback =
  let acceptNext accepted' (sock, _) = do
        _            <- P.version sock
        (incomingSock, incomingDest) <- P.acceptStream sessionId sock
        putMVar accepted' True
        callback (incomingSock, incomingDest)

      acceptFork = liftIO $ do
          accepted' <- newEmptyMVar
          _ <- forkIO $ P.connect "127.0.0.1" "7656" (acceptNext accepted')

          -- This blocks until an actual connection is accepted, so we ensure there
          -- is always just one thread waiting for a new connection.
          _ <- takeMVar accepted'
          return ()

  in forever acceptFork

-- | Starts a server to accept 'S.DatagramAnonymous' or 'D.DatagramRepliable'
--   connections from other hosts and handles them concurrently in different
--   threads. Any acquired resources are cleaned up when the computation ends.
serveDatagram :: ( MonadIO m
                 , MonadMask m)
              => S.Context
              -> ((BS.ByteString, Maybe D.PublicDestination) -> IO ())
              -> m ()
serveDatagram (S.Context sock _ _ _ _) callback =
  let receiveNext received' = do
        res  <- P.receiveDatagram sock
        putMVar received' True
        callback res

      receiveFork = liftIO $ do
          received' <- newEmptyMVar
          _ <- forkIO $ receiveNext received'

          -- This blocks until an actual datagram is received, so we ensure there
          -- is always just one thread waiting for a new connection.
          _ <- takeMVar received'
          return ()

  in forever receiveFork

-- | Connects to a remote 'S.VirtualStream' host. Any acquired resources are
--   cleaned up when the computation ends. Automatically creates a local return
--   destination required for bi-directional communication.
connectStream :: ( MonadIO m
                 , MonadMask m)
              => S.Context                                        -- ^ Our state
              -> D.PublicDestination                              -- ^ Destination to connect to.
              -> ((Network.Socket, D.PublicDestination) -> IO ()) -- ^ Computation to run once connection has been established.
              -> m ()
connectStream (S.Context _ _ sessionId _ _) remoteDestination callback =
  let connectAddress (sock, _) = do
        -- Connect to the remote destination within our session.
        _         <- P.version sock
        _         <- P.connectStream sessionId remoteDestination sock

        -- Any data that is transmitted through the socket at this point is between
        -- the 'localDestination' and the 'remoteDestination'
        liftIO $ callback (sock, remoteDestination)

  in P.connect "127.0.0.1" "7656" connectAddress

-- | Sends a datagram to a remote destination. Optionally sends a return address.
sendDatagram :: ( MonadIO m
                , MonadMask m)
             => S.Context           -- ^ Our context
             -> D.PublicDestination -- ^ Destination to send message to
             -> BS.ByteString       -- ^ The message to send
             -> m ()
sendDatagram (S.Context _ _ sessionId _ _) = P.sendDatagram sessionId
