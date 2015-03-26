-- | Session related types
module Network.Anonymous.I2P.Types.Session where

import qualified Network.Anonymous.I2P.Types.Destination as D
import qualified Network.Anonymous.I2P.Types.Socket      as S
import qualified Network.Anonymous.I2P.Types.Sam         as S
import qualified Network.Socket                          as NS

-- | Context object that is required for all functions that operate on top
--   of the SAM bridge.
data Context = Context {
  sam        :: S.EndPoints,          -- ^ Endpoints to our SAM bridge
  conn       :: NS.Socket,            -- ^ Our connection with the SAM bridge
  socketType :: S.SocketType,         -- ^ The type of connection we are managing
  sessionId  :: String,               -- ^ Our session id
  privDest   :: D.PrivateDestination, -- ^ Our private destination
  pubDest    :: D.PublicDestination   -- ^ Our public destination which we can give out to others
  }
