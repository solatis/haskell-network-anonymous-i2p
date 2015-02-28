{-# LANGUAGE OverloadedStrings #-}

module Network.Anonymous.I2P.Protocol.ParserSpec where

import qualified Data.ByteString                         as BS
import           Network.Anonymous.I2P.Protocol.Parser
import           Network.Anonymous.I2P.Protocol.Parser.Ast (Token (..))

import           Test.Hspec
import           Test.Hspec.Attoparsec

testDestination :: BS.ByteString
testDestination = "TedPIHKiYHLLavX~2XgghB-jYBFkwkeztWM5rwyJCO2yR2gT92FcuEahEcTrykTxafzv~4jSQOL5w0EqElqlM~PEFy5~L1pOyGB56-yVd4I-g2fsM9MGKlXNOeQinghKOcfbQx1LVY35-0X5lQSNX-8I~U7Lefukj7gSC5hieWkDS6WiUW6nYw~t061Ra0GXf2qzqFTB4nkQvnCFKaZGtNwOUUpmIbF0OtLyr6TxC7BQKgcg4jyZPS1LaBO6Wev0ZFYiQHLk4S-1LQFBfT13BxN34g-eCInwHlYeMD6NEdiy0BYHhnbBTq02HbgD3FjxW~GBBB-6a~eFABaIiJJ08XR8Mm6KKpNh~gQXut2OLxs55UhEkqk8YmTODrf6yzWzldCdaaAEVMfryO9oniWWCVl1FgLmzUHPGQ3yzvb8OlXiED2hunEfaEg0fg77FRDnYJnDHMF7i5zcUzRGb67rUa1To~H65hR9cFNWTAwX4svC-gRbbvxfi-bthyj-QqeBBQAEAAcAAOEyRS5bFHDrXnWpsjcRvpQj436gS4iCjCzdOohWgeBKC~gfLVY658op9GF6oRJ78ezPN9FBE0JqNrAM75-uL9CIeJd8JUwdldm83RNSVI1ZPZBK-5F3DgIjTsqHDMzQ9xPETiBO2UZZogXSThx9I9uYuAtg296ZhziKjYnl7wi2i3IgQlNbuPW16ajOcNeKnL1OqFipAL9e3k~LEhgBNM3J2hK1M4jO~BQ19TxIXXUfBsHFU4YjwkAOKqOxR1iP8YD~xUSfdtF9mBe6fT8-WW3-n2WgHXiTLW3PJjJuPYM4hNKNmsxsEz5vi~DE6H1pUsPVs2oXFYKZF3EcsKUVaAVWJBarBPuVNYdJgIbgl1~TJeNor8hGQw6rUTJFaZ~jjQ=="

spec :: Spec
spec = do
  describe "parsing quoted values" $ do
    it "it should succeed when providing a doublequoted value" $
      let msg :: BS.ByteString
          msg = "\"foo\""

      in msg ~> quotedValue `shouldParse` "foo"

    it "it should succeed when providing a doublequoted value with spaces" $
      let msg :: BS.ByteString
          msg = "\"foo bar\""

      in msg ~> quotedValue `shouldParse` "foo bar"

    it "it should succeed when providing a doublequoted value with an escaped quote" $
      let msg :: BS.ByteString
          msg = "\"foo \\\" bar\""

      in msg ~> quotedValue `shouldParse` "foo \\\" bar"

    it "it should stop after a doublequoted value has been reached" $
      let msg :: BS.ByteString
          msg = "\"foo bar\" \"baz\""

      in msg ~> quotedValue `shouldParse` "foo bar"

    it "it should succeed when providing a singlequoted value" $
      let msg :: BS.ByteString
          msg = "'foo'"

      in msg ~> quotedValue `shouldParse` "foo"

    it "it should succeed when providing a singlequoted value with spaces" $
      let msg :: BS.ByteString
          msg = "'foo bar'"

      in msg ~> quotedValue `shouldParse` "foo bar"

    it "it should succeed when providing a singlequoted value with an escaped quote" $
      let msg :: BS.ByteString
          msg = "'foo \\' bar'"

      in msg ~> quotedValue `shouldParse` "foo \\' bar"


    it "it should stop after a singlequoted value has been reached" $
      let msg :: BS.ByteString
          msg = "'foo bar' 'baz'"

      in msg ~> quotedValue `shouldParse` "foo bar"

  describe "parsing unquoted values" $ do
    it "it should succeed when providing a simple value" $
      let msg :: BS.ByteString
          msg = "foo"

      in msg ~> unquotedValue `shouldParse` "foo"

    it "it should stop after whitespace" $
      let msg :: BS.ByteString
          msg = "foo bar"

      in msg ~> unquotedValue `shouldParse` "foo"

    it "it should stop after a newline" $
      let msg :: BS.ByteString
          msg = "foo\r\nbar"

      in msg ~> unquotedValue `shouldParse` "foo"

    it "it should eat a destination" $
      testDestination ~> unquotedValue `shouldParse` testDestination

    it "it should eat a destination and not continue with more" $
      BS.concat [testDestination, " foo=bar"] ~> unquotedValue `shouldParse` testDestination

    it "should fail on empty input" $
      unquotedValue `shouldFailOn` BS.empty

  describe "parsing keys" $ do
    it "it should succeed when providing a simple key" $
      let msg :: BS.ByteString
          msg = "foo"

      in msg ~> key `shouldParse` (Token "foo" Nothing)

    it "it should succeed when providing a space separated key" $
      let msg :: BS.ByteString
          msg = "foo bar"

      in msg ~> key `shouldParse` (Token "foo" Nothing)

    it "it should succeed when providing an equals separated key" $
      let msg :: BS.ByteString
          msg = "foo=bar"

      in msg ~> key `shouldParse` (Token "foo" Nothing)

    it "should fail on empty input" $
      key `shouldFailOn` BS.empty

  describe "parsing key/values" $ do
    it "it should succeed when providing a simple key/value" $
      let msg :: BS.ByteString
          msg = "foo=bar"

      in msg ~> keyValue `shouldParse` (Token "foo" (Just "bar"))

    it "it should succeed when providing a key/value where the value contains an equals sign" $
      let msg :: BS.ByteString
          msg = "foo=bar=wombat"

      in msg ~> keyValue `shouldParse` (Token "foo" (Just "bar=wombat"))

    it "it should succeed when providing a destination as value" $
      let msg :: BS.ByteString
          msg = BS.concat ["destination=", testDestination]

      in msg ~> keyValue `shouldParse` (Token "destination" (Just testDestination))

    it "it should succeed when providing a quoted value" $
      let msg :: BS.ByteString
          msg = "foo=\"bar wombat\""

      in msg ~> keyValue `shouldParse` (Token "foo" (Just "bar wombat"))

    it "should fail on empty value" $
      keyValue `shouldFailOn` ("foo=" :: BS.ByteString)

    it "should fail on empty key" $
      keyValue `shouldFailOn` ("=bar" :: BS.ByteString)

  describe "parsing a token" $ do
    it "should parse a key when only providing a key" $
      let msg :: BS.ByteString
          msg = "foo"

      in msg ~> token `shouldParse` (Token "foo" Nothing)

    it "should parse a key/value when providing a key/value" $
      let msg :: BS.ByteString
          msg = "foo=bar"

      in msg ~> token `shouldParse` (Token "foo" (Just "bar"))

    it "should skip any preceding horizontal whitespace" $
      let msg :: BS.ByteString
          msg = "   foo"

      in msg ~> token `shouldParse` (Token "foo" Nothing)

    it "should not skip any preceding newlines" $
      let msg :: BS.ByteString
          msg = "\nfoo"

      in token `shouldFailOn` msg

  describe "parsing many tokens" $ do
    it "should parse multiple keys correctly" $
      let msg :: BS.ByteString
          msg = "foo bar"

      in msg ~> tokens `shouldParse` [(Token "foo" Nothing), (Token "bar" Nothing)]

    it "should parse multiple key/valuess correctly" $
      let msg :: BS.ByteString
          msg = "foo=bar wom=bat"

      in msg ~> tokens `shouldParse` [(Token "foo" (Just "bar")), (Token "wom" (Just "bat"))]

    it "should be able to mix and match key/values and keys" $
      let msg :: BS.ByteString
          msg = "foo=bar wombat foz=baz"

      in msg ~> tokens `shouldParse` [(Token "foo" (Just "bar")), (Token "wombat" Nothing), Token "foz" (Just "baz")]

  describe "parsing a line" $ do
    it "should parse multiple keys correctly" $
      let msg :: BS.ByteString
          msg = "foo bar\n"

      in msg ~> line `shouldParse` [(Token "foo" Nothing), (Token "bar" Nothing)]

    it "should fail when no newline is provided" $
      line `shouldFailOn` ("foo bar" :: BS.ByteString)
