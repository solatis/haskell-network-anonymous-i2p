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

  let connect      = INP.connect host port
      version      = INP.hello
      context s v  = return (T.Context socketType s v)

  in do
    s <- connect
    v <- version s

    D.log ("initialized with version: " ++ show v) (context s v)
