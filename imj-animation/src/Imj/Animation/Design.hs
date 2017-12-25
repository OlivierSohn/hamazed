{-# OPTIONS_HADDOCK prune #-}

{- | The animation framework. Also, the source code of "Imj.Animation" can be
read to see the functions discussed here being used to create concrete animations. -}

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
