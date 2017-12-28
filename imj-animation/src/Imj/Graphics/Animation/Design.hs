
{- | The animation framework. These notions are used in "Imj.Graphics.Animation"
to create concrete animations. -}

module Imj.Graphics.Animation.Design
          ( module Imj.Graphics.Animation.Design.Animation
          , module Imj.Graphics.Animation.Design.Timing
          , module Imj.Graphics.Animation.Design.Color
          -- * Reexports
          , module Imj.Graphics.Color
          , module Imj.Timing
          , module Imj.Iteration
          , Coords
          ) where

import Imj.Geo.Discrete(Coords)
import Imj.Graphics.Animation.Design.Animation
import Imj.Graphics.Animation.Design.Color
import Imj.Graphics.Animation.Design.Timing
import Imj.Graphics.Color
import Imj.Iteration
import Imj.Timing
