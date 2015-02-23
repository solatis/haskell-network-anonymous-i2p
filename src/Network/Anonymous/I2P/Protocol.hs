{-# LANGUAGE OverloadedStrings #-}

-- | Protocol description
--
-- Defines functions that handle the advancing of the SAMv3 protocol.
module Network.Anonymous.I2P.Protocol ( NST.connect
                                      , version
                                      , versionWithConstraint
                                      , session) where

import           Control.Monad.Catch
import           Control.Monad.IO.Class

import qualified Data.Text                               as T
import qualified Data.Text.Encoding                      as TE
import qualified Data.UUID                               as Uuid
import qualified Data.UUID.V4                            as Uuid

import qualified Data.Attoparsec.ByteString              as Atto
import qualified Data.ByteString                         as BS
import qualified Data.ByteString.Char8                   as BS8
import qualified Network.Simple.TCP                      as NST

import qualified Network.Socket                          as Network
import qualified Network.Socket.ByteString               as Network

import qualified Network.Attoparsec                      as NA

import qualified Network.Anonymous.I2P.Internal.Debug    as D
import qualified Network.Anonymous.I2P.Protocol.Parser   as Parser
import qualified Network.Anonymous.I2P.Types.Destination as D
import qualified Network.Anonymous.I2P.Types.Socket      as S

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
     Parser.VersionResultOk v       -> D.log ("got version: " ++ show v)      (return v)
     Parser.VersionResultNone       -> D.log "no good version found"          (fail "No version could be negotiated")
     Parser.VersionResultError msg  -> D.log ("protocol error: " ++ show msg) (fail ("An error occured while negotiating version: " ++ msg))


session :: ( MonadIO m
           , MonadMask m)
        => S.SocketType                       -- ^ I2P socket type to create
        -> (Network.Socket, Network.SockAddr) -- ^ Our connection with SAM bridge
        -> m (String, D.Destination)          -- ^ Our session id and our private destination key
session socketType (s, _) =
  let socketTypeToString :: S.SocketType -> BS.ByteString
      socketTypeToString S.VirtualStream     = "STREAM"
      socketTypeToString S.DatagramRepliable = "DATAGRAM"
      socketTypeToString S.DatagramAnonymous = "RAW"

      createSessionId :: IO String
      createSessionId =
        return . Uuid.toString =<< Uuid.nextRandom

      versionString :: String -> BS.ByteString
      versionString sessionId =
        BS.concat [ "SESSION CREATE STYLE=", socketTypeToString socketType, " "
                  , "ID=\"", BS8.pack sessionId, "\" "
                  , "DESTINATION=TRANSIENT "
                  , "SIGNATURE_TYPE=EDDSA_SHA512_ED25519"
                  , "\n"]

  in do
    sessionId <- liftIO createSessionId
    liftIO $ putStrLn ("created session id: " ++ show sessionId)
    liftIO $ putStrLn ("sending version string: " ++ show (versionString sessionId))
    liftIO $ Network.sendAll s (versionString sessionId)
    liftIO $ putStrLn "sent version string, now parsing response!"
    res <- NA.parseOne s (Atto.parse Parser.session)

    case res of
     Parser.SessionResultOk d           -> D.log ("got destination: " ++ show d)  (return (sessionId, d))
     Parser.SessionResultDuplicatedId   -> D.log "duplicated session id"          (fail "Session id already in use")
     Parser.SessionResultDuplicatedDest -> D.log "duplicated destination"         (fail "Destination already in use")
     Parser.SessionResultInvalidKey     -> D.log "invalid destination id"         (fail "Destination is not a valid private destination key")
     Parser.SessionResultError msg      -> D.log ("protocol error: " ++ show msg) (fail ("An error occured while creating session: " ++ msg))
