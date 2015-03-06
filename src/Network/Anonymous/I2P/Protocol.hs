{-# LANGUAGE OverloadedStrings #-}

-- | Protocol description
--
-- Defines functions that handle the advancing of the SAMv3 protocol.
module Network.Anonymous.I2P.Protocol ( NST.connect
                                      , version
                                      , versionWithConstraint
                                      , createSession
                                      , createSessionWith
                                      , acceptStream
                                      , connectStream) where

import           Control.Monad.Catch
import           Control.Monad.IO.Class

import qualified Data.Text                                 as T
import qualified Data.Text.Encoding                        as TE
import qualified Data.UUID                                 as Uuid
import qualified Data.UUID.V4                              as Uuid

import qualified Data.Attoparsec.ByteString                as Atto
import qualified Data.Attoparsec.ByteString.Char8          as Atto8
import qualified Data.ByteString                           as BS
import qualified Data.ByteString.Char8                     as BS8
import qualified Network.Simple.TCP                        as NST

import qualified Network.Socket                            as Network
import qualified Network.Socket.ByteString                 as Network

import qualified Network.Attoparsec                        as NA

import qualified Network.Anonymous.I2P.Error               as E
import qualified Network.Anonymous.I2P.Internal.Debug      as D
import qualified Network.Anonymous.I2P.Protocol.Parser     as Parser
import qualified Network.Anonymous.I2P.Protocol.Parser.Ast as Ast
import qualified Network.Anonymous.I2P.Types.Destination   as D
import qualified Network.Anonymous.I2P.Types.Socket        as S

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

-- | Performs same handshake as 'version', but with an explicit min/max supported
--   version provided.
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

      versionParser :: Atto.Parser [Integer]
      versionParser = (Atto8.decimal `Atto.sepBy` Atto8.char '.')

  in do
    liftIO $ Network.sendAll s helloString
    res <- NA.parseOne s (Atto.parse Parser.line)

    case res of
     (Ast.Token "HELLO" Nothing : Ast.Token "REPLY" Nothing : xs) ->
       case (Ast.value                 "RESULT"  xs,
             Ast.valueAs versionParser "VERSION" xs) of

        -- This is the normal result, and 'VERSION' will contain our (parsed) version
        (Just ("OK"),        Just v) -> return v
        (Just ("NOVERSION"), _)      -> E.i2pError (E.mkI2PError E.noVersionErrorType)
        _                            -> E.i2pError (E.mkI2PError E.protocolErrorType)
     _                               -> E.i2pError (E.mkI2PError E.protocolErrorType)

-- | Create a session with default parameters provided.
createSession :: ( MonadIO m
           , MonadMask m)
        => S.SocketType                       -- ^ I2P socket type to create
        -> (Network.Socket, Network.SockAddr) -- ^ Our connection with SAM bridge
        -> m (String, D.Destination)          -- ^ Our session id and our private destination key
createSession = createSessionWith Nothing Nothing Nothing

-- | Create a session, and explicitly provide all parameters to use
createSessionWith :: ( MonadIO m
                     , MonadMask m)
                  => Maybe String                       -- ^ Session id to use. If none is provided, a new
                                                        --   unique session id is created.
                  -> Maybe D.Destination                -- ^ Destination to use. If none is provided, a new
                                                        --   unique destination will be created.
                  -> Maybe D.SignatureType              -- ^ If a new destination is to be created, provides
                                                        --   the signature type to use. If none is provided,
                                                        --   the I2P default will be used.
                  -> S.SocketType                       -- ^ I2P socket type to create
                  -> (Network.Socket, Network.SockAddr) -- ^ Our connection with SAM bridge
                  -> m (String, D.Destination)          -- ^ Our session id and our private destination key

-- Specialization where no session is was provided. In this case, we create a
-- new session id based on a UUID, and enter recursion with the fresh session id
-- provided.
createSessionWith Nothing destination signatureType socketType pair = do
  uuid <- liftIO Uuid.nextRandom

  D.log
    ("created session id: " ++ show uuid)
    createSessionWith (Just (Uuid.toString uuid)) destination signatureType socketType pair

createSessionWith (Just sessionId) destination signatureType socketType (s, _) =
  let socketTypeToString :: S.SocketType -> BS.ByteString
      socketTypeToString S.VirtualStream     = "STREAM"
      socketTypeToString S.DatagramRepliable = "DATAGRAM"
      socketTypeToString S.DatagramAnonymous = "RAW"

      destinationToString :: Maybe D.Destination -> Maybe D.SignatureType -> BS.ByteString
      destinationToString (Just (D.Destination d)) _          = d

      destinationToString Nothing Nothing                     = "TRANSIENT"
      destinationToString Nothing (Just D.DsaSha1)            = "TRANSIENT SIGNATURE_TYPE=DSA_SHA1"

      destinationToString Nothing (Just D.EcdsaSha256P256)    = "TRANSIENT SIGNATURE_TYPE=ECDSA_SHA256_P256"
      destinationToString Nothing (Just D.EcdsaSha384P384)    = "TRANSIENT SIGNATURE_TYPE=ECDSA_SHA384_P384"
      destinationToString Nothing (Just D.EcdsaSha512P521)    = "TRANSIENT SIGNATURE_TYPE=ECDSA_SHA512_P521"

      destinationToString Nothing (Just D.RsaSha2562048)      = "TRANSIENT SIGNATURE_TYPE=RSA_SHA256_2048"
      destinationToString Nothing (Just D.RsaSha3843072)      = "TRANSIENT SIGNATURE_TYPE=RSA_SHA384_3072"
      destinationToString Nothing (Just D.RsaSha5124096)      = "TRANSIENT SIGNATURE_TYPE=RSA_SHA512_4096"

      destinationToString Nothing (Just D.EdDsaSha512Ed25519) = "TRANSIENT SIGNATURE_TYPE=EdDSA_SHA512_Ed25519"

      versionString :: String -> BS.ByteString
      versionString sid =
        BS.concat [ "SESSION CREATE STYLE=", socketTypeToString socketType, " "
                  , "ID=", BS8.pack sid, " "
                  , "DESTINATION=", destinationToString destination signatureType
                  , "\n"]

  in do
    liftIO $ putStrLn ("Sending version string: " ++ show (versionString sessionId))
    liftIO $ Network.sendAll s (versionString sessionId)
    res <- NA.parseOne s (Atto.parse Parser.line)

    case res of
     (Ast.Token "SESSION" Nothing : Ast.Token "STATUS" Nothing : xs) ->
       case (Ast.value "RESULT"      xs,
             Ast.value "DESTINATION" xs) of

        -- This is the normal result, and 'VERSION' will contain our (parsed) version
        (Just ("OK"),        Just d)  -> return (sessionId, D.Destination d)
        (Just ("DUPLICATED_ID"),   _) -> E.i2pError (E.mkI2PError E.duplicatedSessionIdErrorType)
        (Just ("DUPLICATED_DEST"), _) -> E.i2pError (E.mkI2PError E.duplicatedDestinationErrorType)
        (Just ("INVALID_KEY"), _)     -> E.i2pError (E.mkI2PError E.invalidKeyErrorType)
        _                             -> E.i2pError (E.mkI2PError E.protocolErrorType)
     _                                -> E.i2pError (E.mkI2PError E.protocolErrorType)

-- | For VirtualStream sockets, accepts one new connection
acceptStream :: ( MonadIO m
                , MonadMask m)
             => String                             -- ^ Our session id
             -> (Network.Socket, Network.SockAddr) -- ^ Our connection with SAM bridge
             -> m ()                               -- ^ Returns as soon as connection has been accepted
acceptStream sessionId (sock, _) =
  let acceptString :: String -> BS.ByteString
      acceptString s =
        BS.concat [ "STREAM ACCEPT "
                  , "ID=", BS8.pack s, " "
                  , "SILENT=FALSE"
                  , "\n"]

  in do
    liftIO $ putStrLn ("Sending acceptString: " ++ show (acceptString sessionId))
    liftIO $ Network.sendAll sock (acceptString sessionId)
    res <- NA.parseOne sock (Atto.parse Parser.line)

    case res of
     (Ast.Token "STREAM" Nothing : Ast.Token "STATUS" Nothing : xs) ->
       case Ast.value "RESULT" xs of

        -- This is the normal result, and 'VERSION' will contain our (parsed) version
        Just ("OK")         -> return ()
        Just ("INVALID_ID") -> E.i2pError (E.mkI2PError E.invalidIdErrorType)
        _                   -> E.i2pError (E.mkI2PError E.protocolErrorType)
     _                      -> E.i2pError (E.mkI2PError E.protocolErrorType)

-- | For VirtualStream sockets, establishes connection with a remote
connectStream :: ( MonadIO m
                 , MonadMask m)
              => String                             -- ^ Our session id
              -> D.Destination                      -- ^ Destination we wish to connect to
              -> (Network.Socket, Network.SockAddr) -- ^ Our connection with SAM bridge
              -> m ()                               -- ^ Returning state
connectStream sessionId (D.Destination destination) (sock, _) =
  let connectString :: String -> BS.ByteString
      connectString s =
        BS.concat [ "STREAM CONNECT "
                  , "ID=", BS8.pack s, " "
                  , "DESTINATION=", destination, " "
                  , "SILENT=false"
                  , "\n"]

  in do
    liftIO $ putStrLn ("Sending connectString: " ++ show (connectString sessionId))
    liftIO $ Network.sendAll sock (connectString sessionId)
    res <- NA.parseOne sock (Atto.parse Parser.line)

    case res of
     (Ast.Token "STREAM" Nothing : Ast.Token "STATUS" Nothing : xs) ->
       case Ast.value "RESULT" xs of

        -- This is the normal result, and 'VERSION' will contain our (parsed) version
        Just ("OK")              -> return ()
        Just ("INVALID_ID")      -> E.i2pError (E.mkI2PError E.invalidIdErrorType)
        Just ("INVALID_KEY")     -> E.i2pError (E.mkI2PError E.invalidKeyErrorType)
        Just ("TIMEOUT")         -> E.i2pError (E.mkI2PError E.timeoutErrorType)
        Just ("CANT_REACH_PEER") -> E.i2pError (E.mkI2PError E.unreachableErrorType)
        _                        -> E.i2pError (E.mkI2PError E.protocolErrorType)
     _                           -> E.i2pError (E.mkI2PError E.protocolErrorType)
