{-# OPTIONS_HADDOCK hide #-}

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

newtype Iteration = Iteration (Speed, Frame) deriving(Show)
newtype Speed = Speed Int deriving(Eq, Show, Num, Integral, Real, Enum, Ord)
newtype Frame = Frame Int deriving(Eq, Show, Num, Integral, Real, Enum, Ord)


zeroIteration :: Speed -> Iteration
zeroIteration s = Iteration (s,zeroFrame)

nextIteration :: Iteration -> Iteration
nextIteration (Iteration(s@(Speed speed), Frame i)) = Iteration (s, Frame (i + speed))

previousIteration :: Iteration -> Iteration
previousIteration (Iteration(s@(Speed speed), Frame i)) = Iteration (s, Frame (i - speed))

zeroFrame :: Frame
zeroFrame = Frame 0
