{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Protocol description
--
-- Defines functions that handle the advancing of the SAMv3 protocol.
module Network.Anonymous.I2P.Internal.Network.Protocol where

import           Control.Monad.Error
import           Control.Monad.Trans.Resource

import qualified Data.Attoparsec.ByteString as Atto
import           Data.Attoparsec.ByteString.Char8 (char,
                                                   decimal,
                                                   sepBy,
                                                   endOfLine)
import qualified Network.Socket as NS
import qualified Network.Attoparsec as NA

import qualified Network.Anonymous.I2P.Internal.Debug          as D
import qualified Network.Anonymous.I2P.Internal.Network.Socket as INS

-- | Establishes connection with SAM bridge and negotiates protocol version
hello :: ( MonadIO m
         , MonadError String m
         , MonadResource m)
      => NS.HostName
      -> NS.PortNumber
      -> m (NS.Socket, [Integer])
hello hostname port =
  let sock = INS.connect' hostname port

      samVersion :: Atto.Parser [Integer]
      samVersion = do
        _       <- "HELLO REPLY RESULT=OK VERSION="
        version <- decimal `sepBy` char '.'
        _       <- endOfLine

        return version

      negotiateVersion s = do
        INS.sendBS s "HELLO VERSION\n"
        D.log "Parsing version" (NA.parseOne s (Atto.parse samVersion))

  in do
    s       <- sock
    version <- negotiateVersion s

    D.log ("parsed vesion: " ++ show version) (return (s, version))
