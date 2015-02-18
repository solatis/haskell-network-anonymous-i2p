module Network.Anonymous.I2P.Internal.Network.ProtocolSpec where

import           Control.Monad.Trans.Resource
import           Control.Monad.Error

import           Network.Anonymous.I2P.Internal.Network.Protocol (connect)

import           Test.Hspec
import           Test.Hspec.Expectations.Contrib

spec :: Spec
spec = do
  describe "when connecting to a SAM bridge" $ do
    it "connecting to the default port should work" $ do
      result <- runResourceT $ runErrorT $ connect "127.0.0.1" 7656
      result `shouldSatisfy` isRight

    it "connecting to a non-existent port should not work" $ do
      result <- runResourceT $ runErrorT $ connect "127.0.0.1" 1234
      result `shouldSatisfy` isLeft
