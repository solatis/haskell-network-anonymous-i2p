-- | Parser defintions
--
-- Defines parsers used by the I2P SAM protocol
--
--   __Warning__: This function is used internally by 'Network.Anonymous.I2P'
--                and using these functions directly is unsupported. The
--                interface of these functions might change at any time without
--                prior notice.
--

module Network.Anonymous.I2P.Protocol.Parser where

import           Control.Applicative                         ((*>), (<$>), (<*),
                                                              (<|>))

import qualified Data.Attoparsec.ByteString                  as Atto
import qualified Data.Attoparsec.ByteString.Char8            as Atto8
import qualified Data.ByteString                             as BS
import qualified Data.ByteString.Char8                       as BS8
import           Data.Word                                   (Word8)
import qualified Network.Anonymous.I2P.Protocol.Parser.Ast   as A

-- | Ascii offset representation of a double quote.
doubleQuote :: Word8
doubleQuote = 34

-- | Ascii offset representation of a single quote.
singleQuote :: Word8
singleQuote = 39

-- | Ascii offset representation of a backslash.
backslash :: Word8
backslash = 92

-- | Ascii offset representation of an equality sign.
equals :: Word8
equals = 61

-- | Parses a single- or double-quoted value, and returns all bytes within the
--   value; the unescaping is beyond the scope of this function (since different
--   unescaping mechanisms might be desired).
--
--   Looking at the SAMv3 code on github, it appears as if the protocol is kind
--   hacked together at the moment: no character escaping is performed at all,
--   and no formal tokens / AST is used.
--
--   So this function already goes way beyond what is required, but it cannot
--   hurt to do so.
quotedValue :: Atto.Parser BS.ByteString
quotedValue =
  let quoted :: Word8                     -- ^ The character used for quoting
             -> Atto.Parser BS.ByteString -- ^ The value inside the quotes, without the surrounding quotes
      quoted c = (Atto.word8 c *> escaped c <* Atto.word8 c)

      -- | Parses an escaped string, with an arbitrary surrounding quote type.
      escaped :: Word8 -> Atto.Parser BS.ByteString
      escaped c = BS8.concat <$> Atto8.many'
                       -- Make sure that we eat pairs of backslashes; this will make sure
                       -- that a string such as "\\\\" is interpreted correctly, and the
                       -- ending quoted will not be interpreted as escaped.
                  (    Atto8.string (BS8.pack "\\\\")

                       -- This eats all escaped quotes and leaves them in tact; the unescaping
                       -- is beyond the scope of this function.
                   <|> Atto8.string (BS.pack [backslash, c])

                       -- And for the rest: eat everything that is not a quote.
                   <|> (BS.singleton <$> Atto.satisfy (/= c)))

  in quoted doubleQuote <|> quoted singleQuote

-- | An unquoted value is "everything until a whitespace or newline is reached".
--   This is pretty broad, but the SAM implementation in I2P just uses a strtok,
--   and is quite hackish.
unquotedValue :: Atto.Parser BS.ByteString
unquotedValue =
  Atto8.takeWhile1 (not . Atto8.isSpace)

-- | Parses either a quoted value or an unquoted value
value :: Atto.Parser BS.ByteString
value =
  quotedValue <|> unquotedValue

-- | Parses key and value
keyValue :: Atto.Parser A.Token
keyValue = do
  A.Token k _ <- key
  _ <- Atto.word8 equals
  v <- value

  return (A.Token k (Just v))

-- | Parses a key, which, after studying the SAMv3 code, is anything until either
--   a space has been reached, or an '=' is reached.
key :: Atto.Parser A.Token
key =
  let isKeyEnd '=' = True
      isKeyEnd c   = Atto8.isSpace c

  in flip A.Token Nothing <$> Atto8.takeWhile1 (not . isKeyEnd)

-- | A Token is either a Key or a Key/Value combination.
token :: Atto.Parser A.Token
token =
  Atto.skipWhile Atto8.isHorizontalSpace *> (keyValue <|> key)

-- | Parser that reads keys or key/values
tokens :: Atto.Parser [A.Token]
tokens =
  Atto.many' token

-- | A generic parser that reads a whole line of key/values and ends in a newline
line :: Atto.Parser A.Line
line =
  tokens <* Atto8.endOfLine
