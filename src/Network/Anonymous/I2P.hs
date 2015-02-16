{-# LANGUAGE FlexibleContexts #-}

-- | Main interface to I2P
module Network.Anonymous.I2P where

import           Control.Monad.Error
import           Control.Monad.Trans.Resource

import qualified Network.Anonymous.I2P.Internal.Debug            as D
import qualified Network.Anonymous.I2P.Types                     as T
import qualified Network.Anonymous.I2P.Internal.Network.Protocol as INP

-- | Establishes connection with I2P service and creates context that we use in
--   other functions.
initialize :: ( MonadIO m
              , MonadError String m
              , MonadResource m)
           => T.HostName
           -> T.PortNumber
           -> T.SocketType
           -> m T.Context
initialize host port socketType =

  let handShake      = INP.hello host port
      context s v    = return (T.Context socketType s v)

  in do
    (socket, version) <- handShake

    D.log ("initialized with version: " ++ show version) (context socket version)
