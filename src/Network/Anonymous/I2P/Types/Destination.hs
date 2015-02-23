-- | I2P destination related types
module Network.Anonymous.I2P.Types.Destination where

import qualified Data.ByteString as BS

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
  base64 :: BS.ByteString -- ^ Base64 representation of the destination
  } deriving (Eq, Show)
