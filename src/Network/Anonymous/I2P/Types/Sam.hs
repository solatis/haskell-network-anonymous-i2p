-- | SAM endpoint types
module Network.Anonymous.I2P.Types.Sam where

import qualified Network.Socket as NS

-- | Alias for a hostname
type HostName = NS.HostName

-- | Alias for a port number
type PortNumber = NS.ServiceName

-- | A SAM endpoint
type EndPoint = (HostName, PortNumber)

-- | Describes all endpoints required by SAM
data EndPoints = EndPoints {
  tcp :: EndPoint,
  udp :: EndPoint
  }
