{-# LANGUAGE FlexibleContexts #-}

-- | Main interface to I2P
module Network.Anonymous.I2P where

import Control.Monad.IO.Class
import Control.Monad.Catch

import qualified Network.Socket                                  as Network

import qualified Network.Anonymous.I2P.Internal.Debug            as D
import qualified Network.Anonymous.I2P.Types                     as T
import qualified Network.Anonymous.I2P.Protocol as INP

-- | Establishes connection with I2P service and creates context that we use in
--   other functions.
withSession :: ( MonadIO m
               , MonadMask m)
            => Network.HostName
            -> Network.ServiceName
            -> T.SocketType
            -> m ()
withSession host port _ =
  let handleSession s = do
        v <- INP.version s
        D.log ("Got version: " ++ show v) (return ())

  in liftIO $ INP.connect host port handleSession
