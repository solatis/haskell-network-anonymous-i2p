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

data VersionResult =
  VersionResultOk [Integer] |
  VersionResultNone         |
  VersionResultError String
  deriving (Show, Eq)

data SessionResult =
  SessionResultOk D.Destination |
  SessionResultDuplicatedId     |
  SessionResultDuplicatedDest   |
  SessionResultInvalidKey       |
  SessionResultError String
  deriving (Show, Eq)

-- | A parser that reads a quoted message
quotedMessage :: Parser String
quotedMessage = string "\"" *> manyTill anyChar (string "\"")

-- | A parser that reads a message until EOL is reached. EOL is *not* consumed.
endOfLineMessage :: Parser BS.ByteString
endOfLineMessage = Atto.takeTill (Atto.inClass "\r\n")

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

session :: Parser SessionResult
session =
  let parseResultInvalidKey :: Parser SessionResult
      parseResultInvalidKey =
        void (
          string "INVALID_KEY") *> pure SessionResultInvalidKey

      parseResultDuplicatedId :: Parser SessionResult
      parseResultDuplicatedId =
        void (
          string "DUPLICATED_ID") *> pure SessionResultDuplicatedId

      parseResultDuplicatedDest :: Parser SessionResult
      parseResultDuplicatedDest =
        void (
          string "DUPLICATED_DEST") *> pure SessionResultDuplicatedDest

      parseResultOk :: Parser SessionResult
      parseResultOk =
        (SessionResultOk . D.Destination) <$> (
          string "OK DESTINATION=" *> endOfLineMessage)

      parseResultError :: Parser SessionResult
      parseResultError =
        SessionResultError <$> (
          string "I2P_ERROR MESSAGE=" *> quotedMessage)

      parseResult :: Parser SessionResult
      parseResult =
        "SESSION STATUS RESULT=" *>

        (     parseResultInvalidKey
          <|> parseResultDuplicatedId
          <|> parseResultDuplicatedDest
          <|> parseResultOk
          <|> parseResultError )
        <* endOfLine

  in parseResult
