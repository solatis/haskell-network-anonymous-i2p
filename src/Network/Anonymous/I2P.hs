{-# LANGUAGE FlexibleContexts #-}

-- | This module provides the main interface for establishing secure and
--   anonymous connections with other hosts on the interface using the
--   Invisible Internet Project (I2P). For more information about the I2P
--   network, see: <https://www.geti2p.net/ geti2p.net>
--
module Network.Anonymous.I2P (
  -- * Introduction to I2P
  -- $i2p-introduction

  -- * Client side
  -- $i2p-client

  -- * Server side
  -- $i2p-server

  -- ** Setting up the context
    defaultEndPoint
  , createDestination
  , withSession
  , withSession'

  -- ** Virtual streams
  , connectStream
  , serveStream

  -- ** Datagrams
  , serveDatagram
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
import qualified Network.Anonymous.I2P.Types.Sam         as S

--------------------------------------------------------------------------------
-- $i2p-introduction
--
-- This module is an implementation of the SAMv3 protocol for I2P. I2P is an
-- internet anonimization network, similar to Tor. Whereas Tor is primarily
-- intended for privately browsing the world wide web, I2P is more application
-- oriented, and is intended for private communication between applications.
--
-- The general idea of the SAM interface to I2P is that you establish a master
-- connection with the SAM bridge, and create new, short-lived connections with
-- the SAM bridge for the communication with the individual peers.
--
-- I2P provides three different ways of communicating with other hosts:
--
--  * __Virtual Streams__: similar to reliable TCP sockets, data is guaranteed to
--    be delivered, and in order. You can accept virtual streams and connect to
--    remote virtual streams, and use regular sockets to transmit the actual
--    messages.
--
--  * __Repliable Datagrams__: unreliable delivery of messages to a remote host,
--    but adds a reply-to address to the message so the remote host can send a
--    message back.
--
--  * __Anonymous Datagrams__: unreliable delivery of messages to a remote host,
--    and the remote host has no way to find out who sent the message.
--
-- Different methods of communication have different performance characteristics,
-- and an application developer should take these into careful consideration when
-- developing an application.
--
--------------------------------------------------------------------------------
-- $i2p-client
--
-- == Virtual Stream
-- Establishing a 'S.VirtualStream' connection with a remote works as follows:
--
-- @
--   main = 'withSession' 'defaultEndPoint' 'S.VirtualStream' withinSession
--
--   where
--     dest :: 'D.PublicDestination'
--     dest = undefined
--
--     withinSession :: 'S.Context' -> IO ()
--     withinSession ctx =
--       'connectStream' ctx dest worker
--
--     worker (sock, addr) = do
--       -- Now you may use sock to communicate with the remote; addr contains
--       -- the address of the remote we are connected to, which on our case
--       -- should match dest.
--       return ()
-- @
--
-- == Datagram
-- Sending a 'S.DatagramRepliable' message to a remote:
--
-- @
--   main = 'withSession' 'defaultEndPoint' 'S.DatagramRepliable' withinSession
--
--   where
--     dest :: 'D.PublicDestination'
--     dest = undefined
--
--     withinSession :: 'S.Context' -> IO ()
--     withinSession ctx = do
--       'sendDatagram' ctx dest \"Hello, anonymous world!\"
--
--       -- SAM requires the master connection of a session to be alive longer
--       -- than any opertions that occur on the session are. Since sending a
--       -- datagram returns before the datagram might be actually handled by
--       -- I2P, it is adviced to wait a little while before closing the session.
--       threadDelay 1000000
-- @
--
--------------------------------------------------------------------------------
-- $i2p-server
--
-- == Virtual Stream
-- Running a server that accepts 'S.VirtualStream' connections.
--
-- @
--   main = 'withSession' 'defaultEndPoint' 'S.VirtualStream' withinSession
--
--   where
--     withinSession :: 'S.Context' -> IO ()
--     withinSession ctx =
--       'serveStream' ctx worker
--
--     worker (sock, addr) = do
--       -- Now you may use sock to communicate with the remote; addr contains
--       -- the address of the remote we are connected to, which we might want
--       -- to store to send back messages asynchronously.
--       return ()
-- @
--
-- == Datagram
-- Receiving 'S.DatagramAnonymous' messages from remotes:
--
-- @
--   main = 'withSession' 'defaultEndPoint' 'S.DatagramAnonymous' withinSession
--
--   where
--     withinSession :: 'S.Context' -> IO ()
--     withinSession ctx =
--       'serveDatagram' ctx worker
--
--     worker (sock, addr) = do
--       -- Now you may use sock to communicate with the remote; addr is an
--       -- instance of the 'Maybe' monad, and since we only accept anonymous
--       -- messages, should always be 'Nothing'.
--       return ()
-- @
--------------------------------------------------------------------------------

-- | The default host/port SAM uses
defaultEndPoint :: S.EndPoints
defaultEndPoint = S.EndPoints ("127.0.0.1", "7656") ("127.0.0.1", "7655")

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
                  => S.EndPoints                                   -- ^ Our SAM bridge endpoints
                  -> Maybe D.SignatureType                         -- ^ Algorithm to use for signature encryption. As per I2P spec defaults to DSA_SHA1.
                  -> m (D.PrivateDestination, D.PublicDestination) -- ^ The private and public destinations.
createDestination (S.EndPoints (tcpHost, tcpPort) _) signatureType =
  P.connect tcpHost tcpPort (\(sock, _) -> do
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
            => S.EndPoints        -- ^ Our SAM bridge endpoints
            -> S.SocketType       -- ^ The type of socket we will be using.
            -> (S.Context -> m a) -- ^ The computation to run
            -> m a
withSession sam socketType callback = do
  destPair <- createDestination sam Nothing
  withSession' sam socketType destPair callback

-- | Alternative implementation of 'withSession' that explicitly accepts a
--   'D.Destination' pair to use to set up the session. This can be useful
--   if you want to use a specific 'D.SignatureType' to create a local
--   endpoint.
withSession' :: ( MonadIO m
                , MonadMask m)
             => S.EndPoints                                 -- ^ Our SAM bridge endpoints
             -> S.SocketType                                -- ^ The type of socket we will be using.
             -> (D.PrivateDestination, D.PublicDestination) -- ^ Destination to use
             -> (S.Context -> m a)                          -- ^ The computation to run
             -> m a
withSession' sam socketType (privDest, pubDest) callback =
  let bindSession (sock, _) = do
        _         <- P.version sock
        sessionId <- P.createSessionWith Nothing privDest socketType sock

        callback (S.Context sam sock socketType sessionId privDest pubDest)

  in P.connect (fst (S.tcp sam)) (snd (S.tcp sam)) bindSession

-- | Starts a server to accept 'S.VirtualStream' connections from other hosts
--   and handles them concurrently in different threads. Any acquired resources
--   are cleaned up when the computation ends.
serveStream :: ( MonadIO m
               , MonadMask m)
            => S.Context
            -> ((Network.Socket, D.PublicDestination) -> IO ())
            -> m ()
serveStream (S.Context (S.EndPoints (tcpHost, tcpPort) _) _ _ sessionId _ _) callback =
  let acceptNext accepted' (sock, _) = do
        _            <- P.version sock
        (incomingSock, incomingDest) <- P.acceptStream sessionId sock
        putMVar accepted' True
        callback (incomingSock, incomingDest)

      acceptFork = liftIO $ do
          accepted' <- newEmptyMVar
          _ <- forkIO $ P.connect tcpHost tcpPort (acceptNext accepted')

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
serveDatagram (S.Context _ sock _ _ _ _) callback =
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
connectStream (S.Context (S.EndPoints (tcpHost, tcpPort) _) _ _ sessionId _ _) remoteDestination callback =
  let connectAddress (sock, _) = do
        -- Connect to the remote destination within our session.
        _         <- P.version sock
        _         <- P.connectStream sessionId remoteDestination sock

        -- Any data that is transmitted through the socket at this point is between
        -- the 'localDestination' and the 'remoteDestination'
        liftIO $ callback (sock, remoteDestination)

  in P.connect tcpHost tcpPort connectAddress

-- | Sends a datagram to a remote destination.
--
--   __Warning__: This function returns before the actual datagram has arrived at
--                and handled by the SAM bridge. If you close the session opened
--                with 'withSession', a race condition will occur where the datagram
--                will possibly arrive /after/ the session has been closed, and as
--                such will never be delivered.
sendDatagram :: ( MonadIO m
                , MonadMask m)
             => S.Context           -- ^ Our context
             -> D.PublicDestination -- ^ Destination to send message to
             -> BS.ByteString       -- ^ The message to send
             -> m ()
sendDatagram (S.Context (S.EndPoints _ udp) _ _ sessionId _ _) = P.sendDatagram udp sessionId
