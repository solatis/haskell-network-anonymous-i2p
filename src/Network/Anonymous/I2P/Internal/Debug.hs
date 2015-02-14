{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}

-- | Debugging helper functions, for internal use only
module Network.Anonymous.I2P.Internal.Debug where

import Debug.Trace (trace)

-- | Alias to Debug.Trace(trace), but disabled in non-debug builds
log :: String -> a -> a

#ifdef DEBUG
log = trace
#else
log _ ret = ret
#endif
