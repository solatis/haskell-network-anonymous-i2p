module Network.Anonymous.I2P.HlintSpec where

import           Test.Hspec
import           Language.Haskell.HLint (hlint)

spec :: Spec
spec = do
  describe "when running all code through hlint" $ do
    it "it should match coding style" $ do
      hints <- hlint ["src"]

      null hints `shouldBe` True
