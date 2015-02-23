module Network.Anonymous.I2P.Util where

import qualified Network.Anonymous.I2P.Error as E

-- | Returns true if exception is of a certain type
isI2PError :: E.I2PErrorType -> E.I2PException -> Bool
isI2PError lhs (E.I2PError rhs) = lhs == rhs
