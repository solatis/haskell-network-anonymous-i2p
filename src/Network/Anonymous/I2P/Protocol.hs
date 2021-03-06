{-# LANGUAGE OverloadedStrings #-}

-- | Protocol description
--
-- Defines functions that handle the advancing of the SAMv3 protocol.
--
--   __Warning__: This function is used internally by 'Network.Anonymous.I2P'
--                and using these functions directly is unsupported. The
--                interface of these functions might change at any time without
--                prior notice.
--
module Network.Anonymous.I2P.Protocol ( NST.connect
                                      , version
                                      , versionWithConstraint
                                      , createDestination
                                      , createSession
                                      , createSessionWith
                                      , acceptStream
                                      , connectStream
                                      , sendDatagram
                                      , receiveDatagram) where

import           Control.Applicative                         ((<*))
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

import qualified Network.Socket                            as Network hiding (recv, send)
import qualified Network.Socket.ByteString                 as Network

import qualified Network.Attoparsec                        as NA

import qualified Network.Anonymous.I2P.Error               as E
import qualified Network.Anonymous.I2P.Internal.Debug      as D
import qualified Network.Anonymous.I2P.Protocol.Parser     as Parser
import qualified Network.Anonymous.I2P.Protocol.Parser.Ast as Ast
import qualified Network.Anonymous.I2P.Types.Destination   as D
import qualified Network.Anonymous.I2P.Types.Socket        as S
import qualified Network.Anonymous.I2P.Types.Sam           as S


-- | According to the I2P protocol, the first two tokens in a response are always
--   in a fixed position, and for each step in the protocol, we expect two very
--   specific keys to be here.
--
--   This is a function that sends a buffer to a socket, waits for and parses the
--   respone, and returns the remaining tokens.
expectResponse :: ( MonadIO m
                  , MonadMask m)
               => Network.Socket
               -> BS.ByteString
               -> (BS.ByteString, BS.ByteString)
               -> m [Ast.Token]
expectResponse sock output (first, second) = do
  liftIO $ D.log
    ("sending to remote: " ++ show output)
    Network.sendAll sock output

  res <- NA.parseOne sock (Atto.parse Parser.line)

  D.log
    ("received response: " ++ show res)
    return ()

  case res of
   (Ast.Token first' Nothing : Ast.Token second' Nothing : xs) -> if first == first' && second == second'
                                                                  then return xs
                                                                  else E.i2pError (E.mkI2PError E.protocolErrorType)
   _                                                           -> E.i2pError (E.mkI2PError E.protocolErrorType)

-- | Announces ourselves with SAM bridge and negotiates protocol version
--
--   Defaults to protocol version 3.1, which is the only one we support at the
--   moment.
version :: ( MonadIO m
           , MonadMask m)
        => Network.Socket                     -- ^ Our connection with SAM bridge
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
                      -> Network.Socket                     -- ^ Our connection with SAM bridge
                      -> m [Integer]                        -- ^ Version agreed upon, stores as a list of integers; for
                                                            --   example, [3,1] means version 3.1
versionWithConstraint (minV, maxV) sock =
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
    res <- expectResponse sock helloString ("HELLO", "REPLY")
    case (Ast.value                 "RESULT"  res,
          Ast.valueAs versionParser "VERSION" res) of

     -- This is the normal result, and 'VERSION' will contain our (parsed) version
     (Just ("OK"),        Just v) -> return v
     (Just ("NOVERSION"), _)      -> E.i2pError (E.mkI2PError E.noVersionErrorType)
     _                            -> E.i2pError (E.mkI2PError E.protocolErrorType)

-- | Creates a new I2P public/private destination pair
createDestination :: ( MonadIO m
                     , MonadMask m)
                  => Maybe D.SignatureType
                  -> Network.Socket
                  -> m (D.PrivateDestination, D.PublicDestination)
createDestination signature sock =
  let signatureToString :: Maybe D.SignatureType -> BS.ByteString
      signatureToString Nothing                     = ""
      signatureToString (Just D.DsaSha1)            = "SIGNATURE_TYPE=DSA_SHA1"

      signatureToString (Just D.EcdsaSha256P256)    = "SIGNATURE_TYPE=ECDSA_SHA256_P256"
      signatureToString (Just D.EcdsaSha384P384)    = "SIGNATURE_TYPE=ECDSA_SHA384_P384"
      signatureToString (Just D.EcdsaSha512P521)    = "SIGNATURE_TYPE=ECDSA_SHA512_P521"

      signatureToString (Just D.RsaSha2562048)      = "SIGNATURE_TYPE=RSA_SHA256_2048"
      signatureToString (Just D.RsaSha3843072)      = "SIGNATURE_TYPE=RSA_SHA384_3072"
      signatureToString (Just D.RsaSha5124096)      = "SIGNATURE_TYPE=RSA_SHA512_4096"

      signatureToString (Just D.EdDsaSha512Ed25519) = "SIGNATURE_TYPE=EdDSA_SHA512_Ed25519"

      createDestinationString :: BS.ByteString
      createDestinationString =
        BS.concat [ "DEST GENERATE "
                  , signatureToString signature
                  , "\n"]

  in do
    res <- expectResponse sock createDestinationString ("DEST", "REPLY")
    case (Ast.value "PRIV" res, Ast.value "PUB" res)  of
     (Just priv, Just pub) -> return (D.PrivateDestination priv, D.PublicDestination pub)
     _                     -> E.i2pError (E.mkI2PError E.protocolErrorType)

-- | Create a session with default parameters provided.
createSession :: ( MonadIO m
                 , MonadMask m)
              => S.SocketType                                          -- ^ I2P socket type to create
              -> Network.Socket                                        -- ^ Our connection with SAM bridge
              -> m (String, D.PrivateDestination, D.PublicDestination) -- ^ Our session id and our private destination key
createSession socketType sock = do
  (privDestination, pubDestination) <- createDestination Nothing sock
  sessionId                         <- createSessionWith Nothing privDestination socketType sock

  return (sessionId, privDestination, pubDestination)

-- | Create a session, and explicitly provide all parameters to use
createSessionWith :: ( MonadIO m
                     , MonadMask m
                     , D.Acceptable d
                     , D.Destination d)
                  => Maybe String                       -- ^ Session id to use. If none is provided, a new
                                                        --   unique session id is created.
                  -> d                                  -- ^ Destination to use.
                  -> S.SocketType                       -- ^ I2P socket type to create
                  -> Network.Socket                     -- ^ Our connection with SAM bridge
                  -> m String                           -- ^ Our session id

-- Specialization where no session is was provided. In this case, we create a
-- new session id based on a UUID, and enter recursion with the fresh session id
-- provided.
createSessionWith Nothing destination socketType sock = do
  uuid <- liftIO Uuid.nextRandom

  D.log
    ("created session id: " ++ show uuid)
    createSessionWith (Just (Uuid.toString uuid)) destination socketType sock

createSessionWith (Just sessionId) destination socketType sock =
  let socketTypeToString :: S.SocketType -> BS.ByteString
      socketTypeToString S.VirtualStream     = "STREAM"
      socketTypeToString S.DatagramRepliable = "DATAGRAM"
      socketTypeToString S.DatagramAnonymous = "RAW"

      sessionString :: String -> BS.ByteString
      sessionString sid =
        BS.concat [ "SESSION CREATE STYLE=", socketTypeToString socketType, " "
                  , "ID=", BS8.pack sid, " "
                  , "DESTINATION=", D.asByteString destination
                  , "\n"]

  in do
    res <- expectResponse sock (sessionString sessionId) ("SESSION", "STATUS")
    case Ast.value "RESULT" res of
     -- This is the normal result, and 'VERSION' will contain our (parsed) version
     Just ("OK")              -> return sessionId
     Just ("DUPLICATED_ID")   -> E.i2pError (E.mkI2PError E.duplicatedSessionIdErrorType)
     Just ("DUPLICATED_DEST") -> E.i2pError (E.mkI2PError E.duplicatedDestinationErrorType)
     Just ("INVALID_KEY")     -> E.i2pError (E.mkI2PError E.invalidKeyErrorType)
     _                        -> E.i2pError (E.mkI2PError E.protocolErrorType)

-- | For VirtualStream sockets, accepts one new connection
acceptStream :: ( MonadIO m
                , MonadMask m)
             => String                                  -- ^ Our session id
             -> Network.Socket                          -- ^ Our connection with SAM bridge
             -> m (Network.Socket, D.PublicDestination) -- ^ Returns as soon as connection has been accepted
acceptStream sessionId sock =
  let acceptString :: String -> BS.ByteString
      acceptString s =
        BS.concat [ "STREAM ACCEPT "
                  , "ID=", BS8.pack s, " "
                  , "SILENT=false"
                  , "\n"]

      -- After a connection has been accepted, the first line denotes the base64
      -- representation of the remote destination.
      readDestination s =
        let lineParser :: Atto.Parser BS.ByteString
            lineParser = Atto8.takeTill (== '\n') <* Atto8.endOfLine

        in do
          buf <- NA.parseOne s (Atto.parse lineParser)
          return (D.PublicDestination buf)

  in do
    res <- expectResponse sock (acceptString sessionId) ("STREAM", "STATUS")
    case Ast.value "RESULT" res of
      -- This is the normal result, and 'VERSION' will contain our (parsed) version
      Just ("OK")         -> do
        dst <- readDestination sock
        return (sock, dst)
      Just ("INVALID_ID") -> E.i2pError (E.mkI2PError E.invalidIdErrorType)
      _                   -> E.i2pError (E.mkI2PError E.protocolErrorType)

-- | For VirtualStream sockets, establishes connection with a remote
connectStream :: ( MonadIO m
                 , MonadMask m
                 , D.Connectable d
                 , D.Destination d)
              => String                             -- ^ Our session id
              -> d                                  -- ^ Destination we wish to connect to
              -> Network.Socket                     -- ^ Our connection with SAM bridge
              -> m ()                               -- ^ Returning state
connectStream sessionId destination sock =
  let connectString :: String -> BS.ByteString
      connectString s =
        BS.concat [ "STREAM CONNECT "
                  , "ID=", BS8.pack s, " "
                  , "DESTINATION=", D.asByteString destination, " "
                  , "SILENT=false"
                  , "\n"]

  in do
    res <- expectResponse sock (connectString sessionId) ("STREAM", "STATUS")
    case Ast.value "RESULT" res of
     -- This is the normal result, and 'VERSION' will contain our (parsed) version
     Just ("OK")              -> return ()
     Just ("INVALID_ID")      -> E.i2pError (E.mkI2PError E.invalidIdErrorType)
     Just ("INVALID_KEY")     -> E.i2pError (E.mkI2PError E.invalidKeyErrorType)
     Just ("TIMEOUT")         -> E.i2pError (E.mkI2PError E.timeoutErrorType)
     Just ("CANT_REACH_PEER") -> E.i2pError (E.mkI2PError E.unreachableErrorType)
     _                        -> E.i2pError (E.mkI2PError E.protocolErrorType)


-- | For DatagramRepliable and DatagramAnonymous, send a message
sendDatagram :: ( MonadIO m
                , MonadMask m
                , D.Connectable d
                , D.Destination d)
             => S.EndPoint                         -- ^ SAM UDP endpoint
             -> String                             -- ^ Our session id
             -> d                                  -- ^ Destination we wish to send message to
             -> BS.ByteString                      -- ^ Message we wish to send
             -> m ()                               -- ^ Returning state
sendDatagram (udpHost, udpPort) sessionId destination message
  | BS.length message > maxLength = E.i2pError (E.mkI2PError E.messageTooLongErrorType)
  | otherwise =
      let sendString =
            BS.concat [ "3.0 "
                      , BS8.pack sessionId, " "
                      , D.asByteString destination, " "
                      , "\n"
                      , message]

      in do
        -- Establish connection to UDP SAM service at port 7655
        addrinfos <- liftIO $ Network.getAddrInfo Nothing (Just udpHost) (Just udpPort)
        let serveraddr = head addrinfos
        sock <- liftIO $ Network.socket (Network.addrFamily serveraddr) Network.Datagram Network.defaultProtocol
        liftIO $ Network.connect sock (Network.addrAddress serveraddr)

        -- And write the message
        liftIO $ Network.sendAll sock sendString
        return ()

  where maxLength = 31744

-- | For DatagramRepliable and DatagramAnonymous, receive a message
receiveDatagram :: ( MonadIO m
                   , MonadMask m)
                => Network.Socket                               -- ^ Our connection with SAM bridge
                -> m (BS.ByteString, Maybe D.PublicDestination) -- ^ Received buffer, possibly with a reply destination
receiveDatagram sock =

  let receive :: Int -> IO BS.ByteString
      receive 0 = return BS.empty
      receive bytes = do
        recv  <- D.log
                   ("Reading " ++ show bytes ++ " bytes as datagram")
                   Network.recv sock bytes

        recv' <- receive (bytes - BS.length recv)

        return (BS.append recv recv')

      handleRepliable tokens =
          case (Ast.value "SIZE" tokens, Ast.value "DESTINATION" tokens) of
           (Just size, Just destination) -> do
             buf <- liftIO $ (receive . read . BS8.unpack) size
             return (buf, Just (D.PublicDestination destination))
           _                             -> E.i2pError (E.mkI2PError E.protocolErrorType)

      handleAnonymous tokens =
          case Ast.value "SIZE" tokens of
           Just size -> do
             buf <- liftIO $ (receive . read . BS8.unpack) size
             return (buf, Nothing)
           _         -> E.i2pError (E.mkI2PError E.protocolErrorType)

  in do
    res <- NA.parseOne sock (Atto.parse Parser.line)

    case res of
     (Ast.Token "DATAGRAM" Nothing : Ast.Token "RECEIVED" Nothing : xs) -> handleRepliable xs
     (Ast.Token "RAW" Nothing      : Ast.Token "RECEIVED" Nothing : xs) -> handleAnonymous xs
     _                                                                  -> E.i2pError (E.mkI2PError E.protocolErrorType)
