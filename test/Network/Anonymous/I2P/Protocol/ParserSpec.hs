{-# LANGUAGE OverloadedStrings #-}

module Network.Anonymous.I2P.Protocol.ParserSpec where

import qualified Data.ByteString as BS
import           Network.Anonymous.I2P.Protocol.Parser

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

  describe "parsing a session response" $ do
    it "should succeed when providing a correct command" $
      let msg :: BS.ByteString
          msg = "SESSION STATUS RESULT=OK DESTINATION=\"fooPrivateKey\"\n"

      in msg ~> session `shouldParse` (SessionResultOk "fooPrivateKey")

    it "should succeed when providing a duplicated id" $
      let msg :: BS.ByteString
          msg = "SESSION STATUS RESULT=DUPLICATED_ID\n"

      in msg ~> session `shouldParse` (SessionResultDuplicatedId)

    it "should succeed when providing a duplicated destination" $
      let msg :: BS.ByteString
          msg = "SESSION STATUS RESULT=DUPLICATED_DEST\n"

      in msg ~> session `shouldParse` (SessionResultDuplicatedDest)

    it "should succeed when providing an invalid key" $
      let msg :: BS.ByteString
          msg = "SESSION STATUS RESULT=INVALID_KEY\n"

      in msg ~> session `shouldParse` (SessionResultInvalidKey)

    it "should succeed when providing an error message" $
      let msg :: BS.ByteString
          msg = "SESSION STATUS RESULT=I2P_ERROR MESSAGE=\"barMessage\"\n"

      in msg ~> session `shouldParse` (SessionResultError "barMessage")
