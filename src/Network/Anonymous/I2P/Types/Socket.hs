-- | Socket related types
module Network.Anonymous.I2P.Types.Socket where

import qualified Network.Socket as NS

-- | Alias for a hostname
type HostName = NS.HostName

-- | Alias for a port number
type PortNumber = NS.PortNumber

-- | Socket types
data SocketType =
  -- | Virtual streams are guaranteed to be sent reliably and in order, with
  -- failure and success notification as soon as it is available. Streams are
  -- bidirectional communication sockets between two I2P destinations, but their
  -- opening has to be requested by one of them.
  VirtualStream |

  -- | While I2P doesn't inherently contain a FROM address, for ease of use
  -- an additional layer is provided as repliable datagrams - unordered
  -- and unreliable messages of up to 31744 bytes that include a FROM
  -- address (leaving up to 1KB for header material).  This FROM address
  -- is authenticated internally by SAM (making use of the destination's
  -- signing key to verify the source) and includes replay prevention.
  DatagramRepliable |

  -- | Squeezing the most out of I2P's bandwidth, SAM allows clients to send
  -- and receive anonymous datagrams, leaving authentication and reply
  -- information up to the client themselves.  These datagrams are
  -- unreliable and unordered, and may be up to 32768 bytes.
  DatagramAnonymous

  deriving (Show)
