
{- | The animation framework. These notions are used in "Imj.Animation"
to create concrete animations. -}

module Imj.Animation.Design
          ( module Imj.Animation.Design.Animation
          , module Imj.Animation.Design.Timing
          , module Imj.Animation.Design.Color
          -- * Reexports
          , module Imj.Color
          , module Imj.Timing
          , module Imj.Iteration
          , Coords
          ) where

import Imj.Animation.Design.Animation
import Imj.Animation.Design.Color
import Imj.Animation.Design.Timing
import Imj.Color
import Imj.Geo.Discrete(Coords)
import Imj.Iteration
import Imj.Timing
