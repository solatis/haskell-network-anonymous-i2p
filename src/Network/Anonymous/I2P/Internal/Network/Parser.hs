{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Utility functions for running a parser against a socket

module Network.Anonymous.I2P.Internal.Network.Parser where

import           Control.Monad.Error

import qualified Data.ByteString            as BS
import qualified Network.Socket             as NS
import qualified Network.Socket.ByteString  as NSB
import qualified Data.Attoparsec.ByteString as Atto

-- | The parsing continuation form of a "Data.Attoparsec" parser.
type ParseC a = BS.ByteString -> Atto.Result a

-- | Consumes input from socket and attempts to parse a structure. This function
--   will terminate if one of three conditions is met:
--
--   * the parser has completed succesfully;
--   * the parser failed with invalid data;
--   * the connection is closed.
--
--   Depending upon the available data on the socket, this function might block.
parse :: ( MonadIO m
         , MonadError String m)
      => BS.ByteString        -- ^ Unconsumed buffer from previous run
      -> NS.Socket            -- ^ Socket to read data from
      -> Atto.Parser a        -- ^ Parser to use
      -> m (BS.ByteString, a) -- ^ Unconsumed buffer along with the parsed value.
parse buffer socket parser =

  let consumeNext b0 p0 = do
        -- Attempt to complete one round of parsing
        next   <- parseNext p0 b0

        case next of
         -- This case means that the parser consumed all the data but did not yet
         -- complete; enter recursion with the new parser state.
         (_,  p1, Nothing)    -> do
           -- Read all bytes currently available on the socket
           b1 <- liftIO $ NSB.recv socket 4096
           consumeNext b1 p1

         -- This case means that the parser has succesfully completed parsing one
         -- object. We possibly return any unconsumed data.
         (b1, _,  Just value) -> return (b1, value)

      parseNext p0 b0 =
        case p0 b0 of

        -- On error, throw error through MonadError
        Atto.Fail    err _ _  -> throwError ("An error occured while parsing input: " ++ show err)
        Atto.Partial p1       -> return (BS.empty, p1, Nothing)
        Atto.Done    b1 value -> return (b1,       p0, Just value)

  in consumeNext buffer (Atto.parse parser)

-- | Similar to parse, but assumes that there will only be enough data for a
--   single succesful parse on the socket. If, after a succesful parse, there is
--   still unconsumed data, an error will be thrown.
parseSingleton :: ( MonadIO m
                  , MonadError String m)
               => NS.Socket     -- ^ Socket to read data from
               -> Atto.Parser a -- ^ Parser to use
               -> m a           -- ^ Parsed value
parseSingleton socket parser = do
  (buffer, value) <- parse BS.empty socket parser

  case BS.uncons buffer of
   Nothing -> return value
   _       -> throwError ("Unconsumed data left on socket: " ++ show buffer)
