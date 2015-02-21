{-# LANGUAGE OverloadedStrings #-}

module Network.Anonymous.I2P.Internal.Network.ProtocolSpec where

import           Control.Monad.IO.Class
import Control.Exception.Base
import           Control.Monad.Catch
import           Control.Concurrent               (ThreadId, forkIO, killThread, threadDelay)

import qualified Network.Socket                   as NS (Socket)
import qualified Network.Simple.TCP               as NS (HostPreference (HostAny), acceptFork, listen, send)

import           Network.Anonymous.I2P.Internal.Network.Protocol (connect, version)

import           Test.Hspec
import           Test.Hspec.Expectations.Contrib

mockServer :: ( MonadIO m
              , MonadMask m)
           => String
           -> (NS.Socket -> IO a)
           -> m ThreadId
mockServer port callback =
  liftIO $ forkIO $ NS.listen "*" port (\(lsock, _) ->
                                         NS.accept lsock (\pair -> do
                                                             _ <- callback (fst pair)
                                                             return ()))


spec :: Spec
spec = do
  describe "when connecting to a SAM bridge" $ do
    it "connecting to the default port should work" $ do
      connect "127.0.0.1" "7656" (\_ -> return ()) `shouldReturn` ()

    it "connecting to a non-existent port should not work" $ do
      (connect "127.0.0.1" "1234" return) `shouldThrow` anyIOException

    it "connecting to an alternative port should work" $
      let serverSock :: NS.Socket
                     -> IO ()
          serverSock s = return ()

      in do
        thread <- mockServer "4321" serverSock
        connect "127.0.0.1" "4321" (\_ -> return ()) `shouldReturn` ()

  describe "when negotiating protocol version" $ do
    it "should use protocol version 3.1 when connecting to SAM" $
      connect "127.0.0.1" "7656" version `shouldReturn` [3,1]

    it "should use throw an error if no correct version can be found" $
      let serverSock = flip NS.send "VERSION REPLY RESULT=NOVERSION\n"

      in do
        thread <- liftIO $ mockServer "4322" serverSock
        connect "127.0.0.1" "4322" version `shouldThrow` anyIOException

    it "should throw an error when the host sends an incomplete reply" $
      let serverSock = flip NS.send "VERSION REPLY "

      in do
        thread <- liftIO $ mockServer "4323" serverSock
        connect "127.0.0.1" "4323" version `shouldThrow` anyIOException
