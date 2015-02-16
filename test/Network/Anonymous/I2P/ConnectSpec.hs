module Network.Anonymous.I2P.ConnectSpec where

import           Control.Monad.Trans.Resource
import           Control.Monad.Error

import Data.Either (isRight, isLeft)

import           Network.Anonymous.I2P        (initialize)
import           Network.Anonymous.I2P.Types

import           Test.Hspec

spec :: Spec
spec = do
  describe "when initializing a context" $ do
    it "SAM should be listening at its default port" $ do
      result <- runResourceT $ runErrorT $ initialize "127.0.0.1" 7656 VirtualStream
      putStrLn ("result = " ++ show result)
      isRight result `shouldBe` True

    it "SAM should not be listening at another port" $ do
      result <- runResourceT $ runErrorT $ initialize "127.0.0.1" 1234 VirtualStream
      putStrLn ("result = " ++ show result)
      isLeft result `shouldBe` True
