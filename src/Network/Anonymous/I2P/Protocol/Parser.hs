{-# LANGUAGE OverloadedStrings #-}

-- | Parser defintions
--
-- Defines parsers used by the I2P SAM protocol

module Network.Anonymous.I2P.Protocol.Parser where

import Control.Monad (void)
import Control.Applicative ((*>),
                            (<*),
                            (<$>),
                            (<|>),
                            pure)

import qualified Data.ByteString as BS
import           Data.Attoparsec.ByteString as Atto
import           Data.Attoparsec.ByteString.Char8 as Atto8

data VersionResult =
  VersionResultOk [Integer] |
  VersionResultNone         |
  VersionResultError String
  deriving (Show, Eq)

data SessionResult =
  SessionResultOk BS.ByteString |
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
  let parseResultOk :: Parser VersionResult
      parseResultOk   =
        VersionResultOk <$> (string "OK VERSION=" *> decimal `sepBy` char '.')

      parseResultNone :: Parser VersionResult
      parseResultNone =
        void (string "NOVERSION") *> pure VersionResultNone

      parseResultError :: Parser VersionResult
      parseResultError =
        VersionResultError <$> (string "I2P_ERROR MESSAGE=" *> quotedMessage)

      parseResult :: Parser VersionResult
      parseResult =
        "HELLO REPLY RESULT=" *>
        (     parseResultOk
          <|> parseResultNone
          <|> parseResultError )
        <* endOfLine

  in parseResult

session :: Parser SessionResult
session =
  let parseResultOk :: Parser SessionResult
      parseResultOk =
        SessionResultOk <$> (string "OK DESTINATION=" *> endOfLineMessage)

      parseResultDuplicatedId :: Parser SessionResult
      parseResultDuplicatedId =
        void (string "DUPLICATED_ID") *> pure SessionResultDuplicatedId

      parseResultDuplicatedDest :: Parser SessionResult
      parseResultDuplicatedDest =
        void (string "DUPLICATED_DEST") *> pure SessionResultDuplicatedDest

      parseResultInvalidKey :: Parser SessionResult
      parseResultInvalidKey =
        void (string "INVALID_KEY") *> pure SessionResultInvalidKey

      parseResultError :: Parser SessionResult
      parseResultError =
        SessionResultError <$> (string "I2P_ERROR MESSAGE=" *> quotedMessage)

      parseResult :: Parser SessionResult
      parseResult =
        "SESSION STATUS RESULT=" *>
        (     parseResultOk
          <|> parseResultDuplicatedId
          <|> parseResultDuplicatedDest
          <|> parseResultInvalidKey
          <|> parseResultError )
        <* endOfLine

  in parseResult
