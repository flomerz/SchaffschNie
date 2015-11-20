module Game.Output.Util
    ( unmanagedSurface
    ) where

import Foreign.Ptr (Ptr)
import SDL
import SDL.Raw.Types

-- | A helper for unmanaged 'Surface's, since it is not exposed by SDL itself.
unmanagedSurface :: Ptr SDL.Raw.Types.Surface -> SDL.Surface
unmanagedSurface p = SDL.Surface p Nothing
