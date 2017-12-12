module Render.Types
            ( RenderSize(..)
            -- | Reexports
            , Col(..)
            , Row(..)
            ) where

import           Geo.Discrete.Types(Col(..), Row(..))

data RenderSize = FollowsTerminalSize
                | Fixed !Col !Row
