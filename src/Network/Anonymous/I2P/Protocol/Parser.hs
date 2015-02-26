{-# LANGUAGE OverloadedStrings #-}

-- | Parser defintions
--
-- Defines parsers used by the I2P SAM protocol

module Network.Anonymous.I2P.Protocol.Parser where

import           Control.Applicative                     (pure, (*>), (<$>),
                                                          (<*), (<|>))
import           Control.Monad                           (void)

import           Data.Attoparsec.ByteString              as Atto
import           Data.Attoparsec.ByteString.Char8        as Atto8
import qualified Data.ByteString                         as BS
import qualified Network.Anonymous.I2P.Types.Destination as D

-- | Result emitted by 'version'
data VersionResult =
  VersionResultOk [Integer] |
  VersionResultNone         |
  VersionResultError String
  deriving (Show, Eq)

-- | Result emitted by 'createSession'
data CreateSessionResult =
  CreateSessionResultOk D.Destination |
  CreateSessionResultDuplicatedId     |
  CreateSessionResultDuplicatedDest   |
  CreateSessionResultInvalidKey       |
  CreateSessionResultError String
  deriving (Show, Eq)

-- | Result emitted by 'acceptStream'
data AcceptStreamResult =
  AcceptStreamResultOk               |
  AcceptStreamResultInvalidId String |
  AcceptStreamResultError String
  deriving (Show, Eq)

-- | Result emitted by 'connectStream'
data ConnectStreamResult =
  ConnectStreamResultOk                |
  ConnectStreamResultTimeout           |
  ConnectStreamResultUnreachable       |
  ConnectStreamResultInvalidId         |
  ConnectStreamResultInvalidKey        |
  ConnectStreamResultError String
  deriving (Show, Eq)

-- | A parser that reads a quoted message
quotedMessage :: Parser String
quotedMessage = string "\"" *> manyTill anyChar (string "\"")

-- | A parser that reads a message until EOL is reached. EOL is *not* consumed.
endOfLineMessage :: Parser BS.ByteString
endOfLineMessage = Atto.takeTill (Atto.inClass "\r\n")

-- | Parses a HELLO VERSION response
version :: Parser VersionResult
version =
  let parseResultNone :: Parser VersionResult
      parseResultNone =
        void (
          string "NOVERSION") *> pure VersionResultNone

      parseResultOk :: Parser VersionResult
      parseResultOk   =
        VersionResultOk <$> (
          string "OK VERSION=" *> decimal `sepBy` char '.')

      parseResultError :: Parser VersionResult
      parseResultError =
        VersionResultError <$> (
          string "I2P_ERROR MESSAGE=" *> quotedMessage)

      parseResult :: Parser VersionResult
      parseResult =
        "HELLO REPLY RESULT=" *>
        (     parseResultNone
          <|> parseResultOk
          <|> parseResultError )
        <* endOfLine

  in parseResult

-- | Parses a SESSION CREATE response
createSession :: Parser CreateSessionResult
createSession =
  let parseResultInvalidKey :: Parser CreateSessionResult
      parseResultInvalidKey =
        void (
          string "INVALID_KEY") *> pure CreateSessionResultInvalidKey

      parseResultDuplicatedId :: Parser CreateSessionResult
      parseResultDuplicatedId =
        void (
          string "DUPLICATED_ID") *> pure CreateSessionResultDuplicatedId

      parseResultDuplicatedDest :: Parser CreateSessionResult
      parseResultDuplicatedDest =
        void (
          string "DUPLICATED_DEST") *> pure CreateSessionResultDuplicatedDest

      parseResultOk :: Parser CreateSessionResult
      parseResultOk =
        (CreateSessionResultOk . D.Destination) <$> (
          string "OK DESTINATION=" *> endOfLineMessage)

      parseResultError :: Parser CreateSessionResult
      parseResultError =
        CreateSessionResultError <$> (
          string "I2P_ERROR MESSAGE=" *> quotedMessage)

      parseResult :: Parser CreateSessionResult
      parseResult =
        "SESSION STATUS RESULT=" *>

        (     parseResultInvalidKey
          <|> parseResultDuplicatedId
          <|> parseResultDuplicatedDest
          <|> parseResultOk
          <|> parseResultError )
        <* endOfLine

  in parseResult

-- | Parses a STREAM ACCEPT response
acceptStream :: Parser AcceptStreamResult
acceptStream =
  let parseResultOk :: Parser AcceptStreamResult
      parseResultOk =
        void (
          string "OK") *> pure AcceptStreamResultOk

      parseResultError :: Parser AcceptStreamResult
      parseResultError =
        AcceptStreamResultError <$> (
          string "I2P_ERROR MESSAGE=" *> quotedMessage)

      parseResultInvalidId :: Parser AcceptStreamResult
      parseResultInvalidId =
        AcceptStreamResultInvalidId <$> (
          string "INVALID_ID MESSAGE=" *> quotedMessage)

      parseResult :: Parser AcceptStreamResult
      parseResult =
        "STREAM STATUS RESULT=" *>

        (     parseResultOk
          <|> parseResultError
          <|> parseResultInvalidId)
        <* endOfLine

  in parseResult

-- | Parses a STREAM CONNECT response
connectStream :: Parser ConnectStreamResult
connectStream =
  let parseResultOk :: Parser ConnectStreamResult
      parseResultOk =
        void (
          string "OK") *> pure ConnectStreamResultOk

      parseResultTimeout :: Parser ConnectStreamResult
      parseResultTimeout =
        void (
          string "TIMEOUT") *> pure ConnectStreamResultTimeout

      parseResultInvalidId :: Parser ConnectStreamResult
      parseResultInvalidId =
        void (
          string "INVALID_ID") *> pure ConnectStreamResultInvalidId

      parseResultInvalidKey :: Parser ConnectStreamResult
      parseResultInvalidKey =
        void (
          string "INVALID_KEY") *> pure ConnectStreamResultInvalidKey

      parseResultUnreachable :: Parser ConnectStreamResult
      parseResultUnreachable =
        void (
          string "CANT_REACH_PEER") *> pure ConnectStreamResultUnreachable

      parseResultError :: Parser ConnectStreamResult
      parseResultError =
        ConnectStreamResultError <$> (
          string "I2P_ERROR MESSAGE=" *> quotedMessage)

      parseResult :: Parser ConnectStreamResult
      parseResult =
        "STREAM STATUS RESULT=" *>

        (     parseResultOk
          <|> parseResultTimeout
          <|> parseResultInvalidId
          <|> parseResultInvalidKey
          <|> parseResultUnreachable
          <|> parseResultError)
        <* endOfLine

  in parseResult
