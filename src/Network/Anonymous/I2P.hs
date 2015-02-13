-- | Main interface to I2P
module Network.Anonymous.I2P where

import qualified Network.Anonymous.I2P.Internal as I

-- | An I2P socket
newtype Socket a = Socket {
  socketType :: I.SocketType
  }

-- | Creates a new socket
create :: I.SocketType -> Socket a
create = Socket
