-- | Types used by various components of the I2P library
module Network.Anonymous.I2P.Types where

import qualified Network.Socket as NS
import qualified Data.ByteString as BS

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

-- | I2P destination
--
--   This type represents the *encoded*, base64 representation of an I2P
--   destination. According to the I2P documentation at this url:
--
--     https://geti2p.net/en/docs/spec/common-structures#struct_Destination
--
--   the destination is a concatenation of the destination, encryption key
--   and the signing key. However, since these are variable length, it is
--   not practical to actually decode them, since we do not have a use for
--   them anyway.
--
--   Furthermore, when testing using the haskell base64 library, apparently
--   the destination is not valid base64 either.
--
--   Long story short, we will just store the destinations as base64.
data Destination = Destination {
  base64 :: BS.ByteString
  } deriving (Eq, Show)
