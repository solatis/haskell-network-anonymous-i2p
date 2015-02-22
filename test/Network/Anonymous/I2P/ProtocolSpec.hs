{-# LANGUAGE OverloadedStrings #-}

module Network.Anonymous.I2P.ProtocolSpec where

import           Control.Monad.IO.Class
import           Control.Monad.Catch
import           Control.Concurrent               (ThreadId, forkIO, killThread, threadDelay)

import qualified Network.Socket                   as NS (Socket)
import qualified Network.Simple.TCP               as NS (accept, listen, send)

import           Network.Anonymous.I2P.Protocol (connect, version)

import           Test.Hspec

mockServer :: ( MonadIO m
              , MonadMask m)
           => String
           -> (NS.Socket -> IO a)
           -> m ThreadId
mockServer port callback = do
  tid <- liftIO $ forkIO $
    NS.listen "*" port (\(lsock, _) -> NS.accept lsock (\pair -> do
                                                           _ <- callback (fst pair)
                                                           threadDelay 1000000
                                                           return ()))

  liftIO $ threadDelay 1000000
  return tid

spec :: Spec
spec = do
  describe "when connecting to a SAM bridge" $ do
    it "connecting to the default port should work" $ do
      connect "127.0.0.1" "7656" (\_ -> return ()) `shouldReturn` ()

    it "connecting to a non-existent port should not work" $ do
      (connect "127.0.0.1" "1234" return) `shouldThrow` anyIOException

  describe "when negotiating protocol version" $ do
    it "should use protocol version 3.1 when connecting to SAM" $
      connect "127.0.0.1" "7656" version `shouldReturn` [3,1]

    it "should return an arbitrary version if we supply it" $
      let serverSock = flip NS.send "HELLO REPLY RESULT=OK VERSION=1.2\n"

      in do
        thread <- liftIO $ mockServer "4322" serverSock
        connect "127.0.0.1" "4322" version `shouldReturn` [1,2]
        killThread thread

    it "should use throw an error if no correct version can be found" $
      let serverSock = flip NS.send "HELLO REPLY RESULT=NOVERSION\n"

      in do
        thread <- liftIO $ mockServer "4323" serverSock
        connect "127.0.0.1" "4323" version `shouldThrow` anyIOException
        killThread thread

    it "should throw an error when the host sends an incomplete reply" $
      let serverSock = flip NS.send "HELLO VERSION REPLY "

      in do
        thread <- liftIO $ mockServer "4324" serverSock
        connect "127.0.0.1" "4324" version `shouldThrow` anyIOException
        killThread thread
