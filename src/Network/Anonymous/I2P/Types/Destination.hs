-- | I2P destination related types
module Network.Anonymous.I2P.Types.Destination where

import qualified Data.ByteString as BS

-- | I2P destination
--
--   This type represents the *encoded*, base64 representation of an I2P
--   destination. According to the <https://geti2p.net/en/docs/spec/common-structures#struct_Destination I2P documentation>
--   the destination is a concatenation of the destination, encryption key and
--   the signing key. However, since these are variable length, it is not
--   practical to actually decode them, since we do not have a use for them
--   anyway.
--
--   Furthermore, when testing using the haskell base64 library, apparently
--   the destination is not valid base64 either.
--
--   Long story short, we will just store the destinations as base64.
data Destination = Destination {
  base64 :: BS.ByteString -- ^ Base64 representation of the destination
  } deriving (Eq, Show)

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
