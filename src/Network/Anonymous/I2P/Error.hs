{-# LANGUAGE DeriveDataTypeable #-}

-- | I2P error types, inspired by System.IO.Error
module Network.Anonymous.I2P.Error where

import Data.Typeable     (Typeable)

import Control.Monad.IO.Class
import Control.Exception (throwIO)
import Control.Exception.Base (Exception)

-- | Error type used
type I2PError = I2PException

-- | Exception that we use to throw. It is the only type of exception
--   we throw, and the type of error is embedded within the exception.
data I2PException = I2PError {
  i2peType :: I2PErrorType -- ^ Our error type
  } deriving (Show, Eq, Typeable)

-- | Derives our I2P exception from the standard exception, which opens it
--   up to being used with all the regular try/catch/bracket/etc functions.
instance Exception I2PException

-- | An abstract type that contains a value for each variant of 'I2PError'
data I2PErrorType
  = NoVersion
  | DuplicatedSessionId
  | DuplicatedDestination
  | InvalidKey
  | InvalidId
  | ProtocolError
  deriving (Show, Eq)

-- | Generates new I2PException
mkI2PError :: I2PErrorType -> I2PError
mkI2PError t = I2PError { i2peType = t }

-- | I2P error when no protocol version can be agreed upon
noVersionErrorType :: I2PErrorType
noVersionErrorType = NoVersion

-- | I2P error when a session id already exists
duplicatedSessionIdErrorType :: I2PErrorType
duplicatedSessionIdErrorType = DuplicatedSessionId

-- | I2P error when a destination already exists
duplicatedDestinationErrorType :: I2PErrorType
duplicatedDestinationErrorType = DuplicatedDestination

-- | I2P error when an invalid (destination) key is used
invalidKeyErrorType :: I2PErrorType
invalidKeyErrorType = InvalidKey

-- | I2P error when an invalid (session) id is used
invalidIdErrorType :: I2PErrorType
invalidIdErrorType = InvalidId

-- | I2P error when communication with the SAM bridge fails
protocolErrorType :: I2PErrorType
protocolErrorType = ProtocolError

-- | Raise an I2P Exception in the IO monad
i2pException :: (MonadIO m)
             => I2PException
             -> m a
i2pException = liftIO . throwIO

-- | Raise an I2P error in the IO monad
i2pError :: (MonadIO m)
         => I2PError
         -> m a
i2pError = i2pException
