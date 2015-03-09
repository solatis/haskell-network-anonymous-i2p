{-# LANGUAGE OverloadedStrings #-}

module Network.Anonymous.I2PSpec where

import           Control.Concurrent.MVar
import           Control.Concurrent                      (ThreadId, forkIO,
                                                          killThread,
                                                          threadDelay)

import           Network.Anonymous.I2P
import           Test.Hspec

spec :: Spec
spec = do
  describe "when serving VirtualStream connections" $ do
    it "source destination should match accepted destination" $
      let storeDestination pubDestValidate' (sock, dest) =
            putMVar pubDestValidate' dest

      in do
        (privDest0, pubDest0) <- createDestination Nothing
        (privDest1, pubDest1) <- createDestination Nothing

        pubDestValidate0' <- newEmptyMVar
        pubDestValidate1' <- newEmptyMVar

        threadId <- forkIO $ serveStream   privDest0          (storeDestination pubDestValidate1')

        threadDelay 30000000
        connectStream privDest1 pubDest0 (storeDestination pubDestValidate0')

        pubDestValidate0 <- takeMVar pubDestValidate0'
        pubDestValidate1 <- takeMVar pubDestValidate1'

        pubDestValidate0 `shouldBe` pubDest0
        pubDestValidate1 `shouldBe` pubDest1

        killThread threadId
