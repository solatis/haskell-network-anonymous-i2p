{-# LANGUAGE OverloadedStrings #-}

-- | Parser defintions
--
-- Defines parsers used by the I2P SAM protocol

module Network.Anonymous.I2P.Internal.Network.Protocol.Parser where

import Control.Monad (void)
import Control.Applicative ((<*>),
                            (*>),
                            (<**>),
                            (<*),
                            (<$>),
                            (<|>),
                            pure)

import qualified Data.ByteString as BS
import           Data.Attoparsec.ByteString
import           Data.Attoparsec.ByteString.Char8

data VersionResult =
  VersionResultOk [Integer] |
  VersionResultNone         |
  VersionResultError String
  deriving (Show, Eq)

quotedMessage :: Parser String
quotedMessage = string "\"" *> manyTill anyChar (string "\"")

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
