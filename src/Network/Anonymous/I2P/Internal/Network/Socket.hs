{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Network utility functions for I2P

module Network.Anonymous.I2P.Internal.Network.Socket where

import qualified Data.ByteString                as BS
import qualified Data.ByteString.Lazy           as BSL
import           Data.Either                    ()

import           Control.Monad.Error
import           Control.Monad.Trans.Resource

import           Control.Exception              (IOException)
import           Control.Exception.Lifted       (try)

import qualified Network.Socket                 as NS
import qualified Network.Socket.ByteString      as NSB
import qualified Network.Socket.ByteString.Lazy as NSBL

import qualified Network.Anonymous.I2P.Internal.Debug as D

-- | Creates new socket, allocates it inside a ResourceT and provides cleanup
--   code to be called when runResourceT is done.
create :: ( MonadIO m
          , MonadResource m)
       => NS.Family
       -> m NS.Socket
create family = do
  (_releaseKey, socket) <- allocate
    (NS.socket family NS.Stream NS.defaultProtocol)
    NS.close

  return socket

-- | Utility function that allows us to connect to a remote host, and returns an
--   error if there is a connection failure. This error is wrapped as a String
--   using MonadError.
--
--   This is a blocking operation.
connect :: ( MonadIO m
           , MonadError String m
           , MonadResource m)
        => NS.HostName
        -> NS.PortNumber
        -> m (NS.Socket, NS.SockAddr)
connect host port =
  let portToService :: NS.PortNumber -> NS.ServiceName
      portToService = show

      addrInfo :: IO NS.AddrInfo
      addrInfo = do
          (addr:_) <- D.log ("Resolving host " ++ show host ++ " and port " ++ show port) (NS.getAddrInfo Nothing (Just host) (Just (portToService port)))
          D.log ("Resolved address: " ++ show addr) (return addr)

      sockAddr :: NS.AddrInfo -> NS.SockAddr
      sockAddr = NS.addrAddress

      doConnect :: ResourceT IO (NS.Socket, NS.SockAddr)
      doConnect = do
        info   <- liftIO addrInfo
        socket <- create (NS.addrFamily info)

        let addr = sockAddr info

        D.log ("Now connecting to socket at address: " ++ show addr) (liftIO $ NS.connect socket addr)

        return (socket, addr)

      tryConnect :: ResourceT IO (Either IOException (NS.Socket, NS.SockAddr))
      tryConnect = try doConnect

  in
   -- We're converting an Either IOException to a MonadError String here.
   either

     -- If we have a Left, let's convert the IOException to a String and call
     -- call throwError to invocate the MonadError.
     (throwError . show)

     -- Otherwise we can just return
     return

     -- We are just interested in what the ResourceT wraps, not the actual
     -- resource.
     =<< liftResourceT tryConnect


-- | Alternative implementation of connect that only returns the socket we're
--   connected to.
connect' :: ( MonadIO m
            , MonadError String m
            , MonadResource m)
        => NS.HostName
        -> NS.PortNumber
        -> m NS.Socket
connect' host port = do
  (s, _) <- connect host port
  return s

-- | Puts strict ByteString on socket. Depending upon the size of the message,
--   might block.
sendBS :: (MonadIO m)
       => NS.Socket
       -> BS.ByteString
       -> m ()
sendBS socket msg =
  D.log
    ("Now sending over socket: " ++ show msg)
    (liftIO $ NSB.sendAll socket msg)

-- | Reads all bytes currently available
readAvailable :: ( MonadIO m
                 , MonadError String m)
              => NS.Socket
              -> m BS.ByteString
readAvailable socket = do
  buffer <- liftIO $ NSB.recv socket 4096

  -- Since we're using TCP connections, according to the documentation, a 0-byte
  -- return value means that the connection is closed.
  if BS.null buffer
    then throwError "Remote has closed the connection"
    else return buffer
