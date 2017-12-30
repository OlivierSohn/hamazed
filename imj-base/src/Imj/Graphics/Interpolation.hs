{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Graphics.Interpolation
        ( -- * Discrete interpolation and morphing
        {- |
        * 'DiscreteInterpolation' describes interpolation /by value/
        , where the result of the interpolation between two \(v\) is a \(v\)
        * 'DiscreteMorphing' and 'DiscreteColorableMorphing' describe a morphing between
        /drawn/ representations of \(v\).

        These classes rely on the 'DiscreteDistance' class:
        -}
          DiscreteDistance(..)
        , Successive(..)
          -- ** Interpolation
        , DiscreteInterpolation(..)
          -- ** Morphing
        , DiscreteMorphing(..)
        , DiscreteColorableMorphing(..)
          -- * Lists interpolations
          {-| The 'DiscreteInterpolation' instance on [] defines a parallel
          interpolation (interpolation occurs at the same time for all same-index
          elements).

          To interpolate sequentially (i.e one index at a time), use
          'SequentiallyInterpolatedList' instead:-}
        , SequentiallyInterpolatedList(..)
        , module Imj.Graphics.Interpolation.Evolution
         -- * Reexports
         , module Imj.Iteration
        ) where

import           Imj.Graphics.Class.DiscreteInterpolation
import           Imj.Graphics.Class.DiscreteColorableMorphing
import           Imj.Graphics.Interpolation.Evolution
import           Imj.Graphics.Interpolation.SequentiallyInterpolatedList
import           Imj.Iteration
