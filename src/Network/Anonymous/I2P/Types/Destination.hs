-- | I2P destination related types
module Network.Anonymous.I2P.Types.Destination where

import qualified Data.ByteString as BS

-- | Interface for any destination
class Destination a where
  -- | Any destination should be convertable to a ByteString in order to
  --   send it over a socket.
  asByteString :: a -> BS.ByteString

-- | An I2P destination we can connect to.
class Connectable a where

-- | An I2P destination we can accept connections from.
class Acceptable  a where

-- | I2P Public destination
--
--   A public destination is the base64 representation of the public I2P
--   key of a destination, and should be given out to other people to connect
--   to your host.
data PublicDestination = PublicDestination BS.ByteString deriving (Eq, Show)

-- | We can connect to a public destination
instance Connectable PublicDestination

-- | A public destination is a 'Destination' and can be converted to a ByteString.
instance Destination PublicDestination where
  asByteString (PublicDestination bs) = bs

-- | I2P Private destination
--
--   A private destination is the base64 representation of the private I2P
--   key of a destination, and you should keep this address to yourself. It
--   can be used to accepts connections, and as such, if you give this private
--   destination out to others, you are effectively giving them the ability
--   to MITM you.
data PrivateDestination = PrivateDestination BS.ByteString deriving (Eq, Show)

-- | We can connect to a private destination
instance Connectable PrivateDestination

-- | We can accept connections at a private destination
instance Acceptable  PrivateDestination

-- | A private destination is a 'Destination' and can be converted to a ByteString.
instance Destination PrivateDestination where
  asByteString (PrivateDestination bs) = bs

-- | Supported signature types by I2P, as defined at
--   <https://geti2p.net/en/docs/spec/common-structures#type_Signature I2P Common Structure Documentation>
data SignatureType =
  -- | DSA_SHA1 -- the default, and supported by all I2P versions
  DsaSha1            |

  -- | ECDSA_SHA256_P256, supported by version 0.9.12 and up
  EcdsaSha256P256    |

  -- | ECDSA_SHA384_P384, supported by version 0.9.12 and up
  EcdsaSha384P384    |

  -- | ECDSA_SHA512_P521, supported by version 0.9.12 and up
  EcdsaSha512P521    |

  -- | RSA_SHA256_2048, supported by version 0.9.12 and up
  RsaSha2562048      |

  -- | RSA_SHA384_3072, supported by version 0.9.12 and up
  RsaSha3843072      |

  -- | RSA_SHA512_4096, supported by version 0.9.12 and up
  RsaSha5124096      |

  -- | EdDSA_SHA512_Ed25519, supported by version 0.9.15 and up
  EdDsaSha512Ed25519
