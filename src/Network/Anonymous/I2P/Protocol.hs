{-# LANGUAGE OverloadedStrings #-}

-- | Protocol description
--
-- Defines functions that handle the advancing of the SAMv3 protocol.
module Network.Anonymous.I2P.Protocol (NST.connect, version, versionWithConstraint) where

import           Control.Monad.Catch
import           Control.Monad.IO.Class

import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import qualified Data.ByteString as BS
import qualified Data.Attoparsec.ByteString            as Atto
import qualified Network.Simple.TCP                    as NST

import qualified Network.Socket                        as Network
import qualified Network.Socket.ByteString             as Network

import qualified Network.Attoparsec                    as NA

import qualified Network.Anonymous.I2P.Protocol.Parser as Parser
import qualified Network.Anonymous.I2P.Internal.Debug  as D

-- | Announces ourselves with SAM bridge and negotiates protocol version
--
--   Defaults to protocol version 3.1, which is the only one we support at the
--   moment.
version :: ( MonadIO m
           , MonadMask m)
        => (Network.Socket, Network.SockAddr) -- ^ Our connection with SAM bridge
        -> m [Integer]                        -- ^ Version agreed upon, stores as a list of integers; for
                                              --   example, [3,1] means version 3.1
version = versionWithConstraint ([3,1], [3,1])

versionWithConstraint :: ( MonadIO m
                         , MonadMask m)
                      => ([Integer], [Integer])             -- ^ Min/max version we want to agree on, stored as a list
                                                            --   of integers. For example, ([3,0], [3,1]) means min
                                                            --   version 3.0, max version 3.1
                      -> (Network.Socket, Network.SockAddr) -- ^ Our connection with SAM bridge
                      -> m [Integer]                        -- ^ Version agreed upon, stores as a list of integers; for
                                                            --   example, [3,1] means version 3.1
versionWithConstraint (minV, maxV) (s, _) =
  let versionToString :: [Integer] -> BS.ByteString
      versionToString vs =
        let textList :: [Integer] -> [T.Text]
            textList = map (T.pack . show)

            versionify :: [T.Text] -> T.Text
            versionify = T.intercalate "."

        in TE.encodeUtf8 (versionify (textList vs))

      helloString :: BS.ByteString
      helloString = BS.concat ["HELLO VERSION MIN=", versionToString minV, " MAX=", versionToString maxV, "\n"]

  in do
    liftIO $ Network.sendAll s helloString
    res <- NA.parseOne s (Atto.parse Parser.version)

    case res of
     Parser.VersionResultOk v       -> D.log ("got version: " ++ show v) (return v)
     Parser.VersionResultNone       -> D.log ("no good version found") (fail "No version could be negotiated")
     Parser.VersionResultError msg  -> D.log ("protocol error: " ++ show msg) (fail ("An error occured while negotiating version: " ++ msg))
