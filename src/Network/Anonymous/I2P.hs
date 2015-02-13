-- | Main interface to I2P
module Network.Anonymous.I2P where

import qualified Network.Anonymous.I2P.Types as T

-- | An I2P socket
newtype Socket a = Socket {
  socketType :: T.SocketType
  }

-- | Creates a new socket
create :: T.SocketType -> Socket a
create = Socket
