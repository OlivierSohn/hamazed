{-# OPTIONS_HADDOCK prune #-}

{-# LANGUAGE GeneralizedNewtypeDeriving, NoImplicitPrelude #-}

module Iteration
           ( Iteration(..)
           , Speed(..)
           , Frame(..)
           , zeroFrame
           , zeroIteration
           , nextIteration
           , previousIteration
           ) where

import Imajuscule.Prelude

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
