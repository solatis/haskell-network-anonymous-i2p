{-# LANGUAGE FlexibleContexts #-}

-- | Main interface to I2P
module Network.Anonymous.I2P where

import           Control.Monad.Catch
import           Control.Monad.IO.Class

import qualified Network.Socket                       as Network

import qualified Network.Anonymous.I2P.Internal.Debug as D
import qualified Network.Anonymous.I2P.Protocol       as INP
import qualified Network.Anonymous.I2P.Types.Socket   as Socket

-- | Establishes connection with I2P service and creates context that we use in
--   other functions.
withSession :: ( MonadIO m
               , MonadMask m)
            => Network.HostName
            -> Network.ServiceName
            -> Socket.SocketType
            -> m ()
withSession host port _ =
  let handleSession s = do
        v <- INP.version s
        D.log ("Got version: " ++ show v) (return ())

  in liftIO $ INP.connect host port handleSession
