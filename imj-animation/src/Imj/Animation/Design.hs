{-# OPTIONS_HADDOCK prune #-}

{- | Animation framework.

"Imj.Animation" contains functions that make use of the
notions described here. -}

module Imj.Animation.Design
          ( module Imj.Animation.Design.Types
          , module Imj.Animation.Design.Timing
          , module Imj.Animation.Design.Color
          -- * Reexports
          , module Imj.Color
          , module Imj.Timing
          , module Imj.Iteration
          , Coords
          ) where

import Imj.Animation.Design.Color
import Imj.Animation.Design.Timing
import Imj.Animation.Design.Types
import Imj.Color
import Imj.Geo.Discrete(Coords)
import Imj.Iteration
import Imj.Timing
