{-# LANGUAGE FlexibleContexts #-}

-- | Main interface to I2P
module Network.Anonymous.I2P ( createDestination
                             , serveStream
                             , connectStream
                             , connectStream') where

import           Control.Monad                           (forever)
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Concurrent (forkIO)

import qualified Network.Socket                          as Network

import qualified Network.Anonymous.I2P.Protocol          as P
import qualified Network.Anonymous.I2P.Types.Destination as D
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
  P.connect "127.0.0.1" "7656" (\pair -> do
                                   _ <- P.version pair
                                   P.createDestination signatureType pair)

-- | Start a server to accept 'S.VirtualStream' connections from other hosts and
--   handles them concurrently in different threads. Any acquired resources are
--   cleaned up when the computation ends.
serveStream :: ( MonadIO m
               , MonadMask m)
            => D.PrivateDestination                             -- ^ Destination we will accept connections at.
            -> ((Network.Socket, D.PublicDestination) -> IO ()) -- ^ Computation to run for accepted connection in a different thread.
            -> m ()
serveStream localDestination callback =
  let acceptNext sessionId pair = do
        _            <- P.version pair
        incomingPair <- P.acceptStream sessionId pair
        callback incomingPair

      acceptFork sessionId =
        liftIO $ P.connect "127.0.0.1" "7656" (acceptNext sessionId)

      bindAddress pair = do

        -- Create new I2P VirtualStream session.
        _         <- P.version pair
        sessionId <- P.createSessionWith Nothing localDestination S.VirtualStream pair

        -- Using this session, enter a never-endling loop accepting incoming I2P
        -- connections on this address.
        forever $ acceptFork sessionId

  in P.connect "127.0.0.1" "7656" bindAddress

-- | Connects to a remote 'S.VirtualStream' host. Any acquired resources are
--   cleaned up when the computation ends. Automatically creates a local return
--   destination required for bi-directional communication.
connectStream :: ( MonadIO m
                 , MonadMask m)
              => D.PublicDestination                              -- ^ Destination to connect to.
              -> ((Network.Socket, D.PublicDestination) -> IO ()) -- ^ Computation to run once connection has been established.
              -> m ()
connectStream remoteDestination callback = do
  (localDestination, _ ) <- createDestination Nothing
  connectStream' localDestination remoteDestination callback

-- | Alternative implementation of connectStream' which requires our local destination
--   to be explicitly provided. This is useful when you want to specifically define
--   a specific 'S.SignatureType' to use for our local destination.
connectStream' :: ( MonadIO m
                  , MonadMask m)
               => D.PrivateDestination                             -- ^ Our local private destination required for bi-directional communication.
               -> D.PublicDestination                              -- ^ Destination to connect to.
               -> ((Network.Socket, D.PublicDestination) -> IO ()) -- ^ Computation to run once connection has been established.
               -> m ()
connectStream' localDestination remoteDestination callback =
  let connectAddress sessionId pair = do
        -- Connect to the remote destination within our session.
        _         <- P.version pair
        _         <- P.connectStream sessionId remoteDestination pair

        -- Any data that is transmitted through the socket at this point is between
        -- the 'localDestination' and the 'remoteDestination'
        liftIO $ callback (fst pair, remoteDestination)

      bindAddress pair = do

        -- Create new I2P VirtualStream session.
        _         <- P.version pair
        sessionId <- P.createSessionWith Nothing localDestination S.VirtualStream pair


        P.connect "127.0.0.1" "7656" (connectAddress sessionId)

  in P.connect "127.0.0.1" "7656" bindAddress
