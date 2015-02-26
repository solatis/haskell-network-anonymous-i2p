{-# LANGUAGE OverloadedStrings #-}

module Network.Anonymous.I2P.Protocol.ParserSpec where

import qualified Data.ByteString                         as BS
import           Network.Anonymous.I2P.Protocol.Parser
import qualified Network.Anonymous.I2P.Types.Destination as D

import           Test.Hspec
import           Test.Hspec.Attoparsec

spec :: Spec
spec = do
  describe "parsing a hello response" $ do
    it "should succeed when providing a correct version" $
      let msg :: BS.ByteString
          msg = "HELLO REPLY RESULT=OK VERSION=3.1\n"

      in msg ~> version `shouldParse` (VersionResultOk [3,1])

    it "should succeed when providing version unknown" $
      let msg :: BS.ByteString
          msg = "HELLO REPLY RESULT=NOVERSION\n"

      in msg ~> version `shouldParse` (VersionResultNone)

    it "should succeed when providing an error message" $
      let msg :: BS.ByteString
          msg = "HELLO REPLY RESULT=I2P_ERROR MESSAGE=\"fooMessage\"\n"

      in do
        msg ~> version `shouldParse` (VersionResultError "fooMessage")

  describe "parsing a create session response" $ do
    it "should succeed when providing a correct command" $
      let msg :: BS.ByteString

          -- This has been copied from an actual reply
          msg = "SESSION STATUS RESULT=OK DESTINATION=TedPIHKiYHLLavX~2XgghB-jYBFkwkeztWM5rwyJCO2yR2gT92FcuEahEcTrykTxafzv~4jSQOL5w0EqElqlM~PEFy5~L1pOyGB56-yVd4I-g2fsM9MGKlXNOeQinghKOcfbQx1LVY35-0X5lQSNX-8I~U7Lefukj7gSC5hieWkDS6WiUW6nYw~t061Ra0GXf2qzqFTB4nkQvnCFKaZGtNwOUUpmIbF0OtLyr6TxC7BQKgcg4jyZPS1LaBO6Wev0ZFYiQHLk4S-1LQFBfT13BxN34g-eCInwHlYeMD6NEdiy0BYHhnbBTq02HbgD3FjxW~GBBB-6a~eFABaIiJJ08XR8Mm6KKpNh~gQXut2OLxs55UhEkqk8YmTODrf6yzWzldCdaaAEVMfryO9oniWWCVl1FgLmzUHPGQ3yzvb8OlXiED2hunEfaEg0fg77FRDnYJnDHMF7i5zcUzRGb67rUa1To~H65hR9cFNWTAwX4svC-gRbbvxfi-bthyj-QqeBBQAEAAcAAOEyRS5bFHDrXnWpsjcRvpQj436gS4iCjCzdOohWgeBKC~gfLVY658op9GF6oRJ78ezPN9FBE0JqNrAM75-uL9CIeJd8JUwdldm83RNSVI1ZPZBK-5F3DgIjTsqHDMzQ9xPETiBO2UZZogXSThx9I9uYuAtg296ZhziKjYnl7wi2i3IgQlNbuPW16ajOcNeKnL1OqFipAL9e3k~LEhgBNM3J2hK1M4jO~BQ19TxIXXUfBsHFU4YjwkAOKqOxR1iP8YD~xUSfdtF9mBe6fT8-WW3-n2WgHXiTLW3PJjJuPYM4hNKNmsxsEz5vi~DE6H1pUsPVs2oXFYKZF3EcsKUVaAVWJBarBPuVNYdJgIbgl1~TJeNor8hGQw6rUTJFaZ~jjQ==\n"

      in msg ~> createSession `shouldParse` (CreateSessionResultOk (D.Destination "TedPIHKiYHLLavX~2XgghB-jYBFkwkeztWM5rwyJCO2yR2gT92FcuEahEcTrykTxafzv~4jSQOL5w0EqElqlM~PEFy5~L1pOyGB56-yVd4I-g2fsM9MGKlXNOeQinghKOcfbQx1LVY35-0X5lQSNX-8I~U7Lefukj7gSC5hieWkDS6WiUW6nYw~t061Ra0GXf2qzqFTB4nkQvnCFKaZGtNwOUUpmIbF0OtLyr6TxC7BQKgcg4jyZPS1LaBO6Wev0ZFYiQHLk4S-1LQFBfT13BxN34g-eCInwHlYeMD6NEdiy0BYHhnbBTq02HbgD3FjxW~GBBB-6a~eFABaIiJJ08XR8Mm6KKpNh~gQXut2OLxs55UhEkqk8YmTODrf6yzWzldCdaaAEVMfryO9oniWWCVl1FgLmzUHPGQ3yzvb8OlXiED2hunEfaEg0fg77FRDnYJnDHMF7i5zcUzRGb67rUa1To~H65hR9cFNWTAwX4svC-gRbbvxfi-bthyj-QqeBBQAEAAcAAOEyRS5bFHDrXnWpsjcRvpQj436gS4iCjCzdOohWgeBKC~gfLVY658op9GF6oRJ78ezPN9FBE0JqNrAM75-uL9CIeJd8JUwdldm83RNSVI1ZPZBK-5F3DgIjTsqHDMzQ9xPETiBO2UZZogXSThx9I9uYuAtg296ZhziKjYnl7wi2i3IgQlNbuPW16ajOcNeKnL1OqFipAL9e3k~LEhgBNM3J2hK1M4jO~BQ19TxIXXUfBsHFU4YjwkAOKqOxR1iP8YD~xUSfdtF9mBe6fT8-WW3-n2WgHXiTLW3PJjJuPYM4hNKNmsxsEz5vi~DE6H1pUsPVs2oXFYKZF3EcsKUVaAVWJBarBPuVNYdJgIbgl1~TJeNor8hGQw6rUTJFaZ~jjQ=="))

    it "should succeed when providing a duplicated id" $
      let msg :: BS.ByteString
          msg = "SESSION STATUS RESULT=DUPLICATED_ID\n"

      in msg ~> createSession `shouldParse` (CreateSessionResultDuplicatedId)

    it "should succeed when providing a duplicated destination" $
      let msg :: BS.ByteString
          msg = "SESSION STATUS RESULT=DUPLICATED_DEST\n"

      in msg ~> createSession `shouldParse` (CreateSessionResultDuplicatedDest)

    it "should succeed when providing an invalid key" $
      let msg :: BS.ByteString
          msg = "SESSION STATUS RESULT=INVALID_KEY\n"

      in msg ~> createSession `shouldParse` (CreateSessionResultInvalidKey)

    it "should succeed when providing an error message" $
      let msg :: BS.ByteString
          msg = "SESSION STATUS RESULT=I2P_ERROR MESSAGE=\"barMessage\"\n"

      in msg ~> createSession `shouldParse` (CreateSessionResultError "barMessage")

  describe "parsing an accept stream response" $ do
    it "should succeed when providing a correct command" $
      let msg :: BS.ByteString
          msg = "STREAM STATUS RESULT=OK\n"

      in msg ~> acceptStream `shouldParse` (AcceptStreamResultOk)

    it "should succeed when providing an invalid id" $
      let msg :: BS.ByteString
          msg = "STREAM STATUS RESULT=INVALID_ID MESSAGE=\"fooBarBaz\"\n"

      in msg ~> acceptStream `shouldParse` (AcceptStreamResultInvalidId "fooBarBaz")

    it "should succeed when providing an error message" $
      let msg :: BS.ByteString
          msg = "STREAM STATUS RESULT=I2P_ERROR MESSAGE=\"wombat\"\n"

      in msg ~> acceptStream `shouldParse` (AcceptStreamResultError "wombat")
