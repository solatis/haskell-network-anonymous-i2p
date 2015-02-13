{-# LANGUAGE FlexibleContexts #-}

-- | Protocol description
--
-- Defines functions that handle the advancing of the SAMv3 protocol.
module Network.Anonymous.I2P.Internal.Network.Protocol where

import           Control.Monad.Error
import           Control.Monad.Trans.Resource

import qualified Network.Socket as NS
import qualified Network.Anonymous.I2P.Internal.Network.Socket as INS

-- | Establishes connection with SAM bridge and negotiates protocol version
hello :: ( MonadIO m
         , MonadError String m
         , MonadResource m)
      => NS.HostName
      -> NS.PortNumber
      -> m NS.Socket
hello hostname port =
  let sock = INS.connect' hostname port

  in sock
