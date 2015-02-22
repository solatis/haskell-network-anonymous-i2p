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
