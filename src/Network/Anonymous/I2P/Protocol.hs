{-# LANGUAGE OverloadedStrings #-}

-- | Protocol description
--
-- Defines functions that handle the advancing of the SAMv3 protocol.
module Network.Anonymous.I2P.Protocol (NST.connect, version) where

import           Control.Monad.Catch
import           Control.Monad.IO.Class

import qualified Data.Attoparsec.ByteString                             as Atto
import qualified Network.Simple.TCP                                     as NST

import qualified Network.Socket                                         as Network
import qualified Network.Socket.ByteString                              as Network

import qualified Network.Attoparsec                                     as NA

import qualified Data.ByteString                                        as BS
import qualified Data.ByteString.Char8                                  as BS8

import qualified Network.Anonymous.I2P.Protocol.Parser as Parser

-- | Announces ourselves with SAM bridge and negotiates protocol version
version :: ( MonadIO m
           , MonadMask m)
        => (Network.Socket, Network.SockAddr) -- ^ Our connection with SAM bridge
        -> m [Integer]                        -- ^ Version agreed upon, stores as a list of integers; for
                                              --   example, [3,1] means version 3.1
version (s, _) = do
  liftIO $ Network.sendAll s "HELLO VERSION\n"
  res <- NA.parseOne s (Atto.parse Parser.version)

  case res of
   Parser.VersionResultOk version -> return version
   Parser.VersionResultNone       -> fail "No version could be negotiated"
   Parser.VersionResultError msg  -> fail ("An error occured while negotiating version: " ++ msg)
