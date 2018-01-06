{-# OPTIONS_HADDOCK prune #-}

-- | Functions and types around the notion of iteration.
--
-- Iterations are used for animations ("Imj.Graphics.ParticleSystem.Design") and
-- evolutions ("Imj.Graphics.Interpolation.Evolution").
{-# LANGUAGE GeneralizedNewtypeDeriving, NoImplicitPrelude #-}

module Imj.Iteration
           ( Iteration(..)
           , Speed(..)
           , Frame(..)
           , zeroFrame
           , zeroIteration
           , nextIteration
           , previousIteration
           ) where

import           Imj.Prelude

-- | An 'Iteration' has a 'Speed' and an iterator: 'Frame'
data Iteration = Iteration !Speed !Frame deriving(Show)

-- | The 'Speed' at which the iteration occurs (if speed > 1, some 'Frame's are skipped).
newtype Speed = Speed Int deriving(Eq, Show, Num, Integral, Real, Enum, Ord)

-- | Iterator of 'Iteration'
newtype Frame = Frame Int deriving(Eq, Show, Num, Integral, Real, Enum, Ord)


zeroIteration :: Speed -> Iteration
zeroIteration s = Iteration s zeroFrame

-- | Iterates forward.
nextIteration :: Iteration -> Iteration
nextIteration (Iteration s@(Speed speed) (Frame i)) = Iteration s (Frame (i + speed))

-- | Iterates backward.
previousIteration :: Iteration -> Iteration
previousIteration (Iteration s@(Speed speed) (Frame i)) = Iteration s (Frame (i - speed))

zeroFrame :: Frame
zeroFrame = Frame 0
