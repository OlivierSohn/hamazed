{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving #-}

module Iteration
           ( Iteration(..)
           , Speed(..)
           , Frame(..)
           , zeroFrame
           , zeroIteration
           , nextIteration
           , previousIteration
           ) where

import           GHC.Generics( Generic )

newtype Iteration = Iteration (Speed, Frame) deriving(Generic, Show)
newtype Speed = Speed Int deriving(Generic, Eq, Show, Num, Integral, Real, Enum, Ord)
newtype Frame = Frame Int deriving(Generic, Eq, Show, Num, Integral, Real, Enum, Ord)


zeroIteration :: Speed -> Iteration
zeroIteration s = Iteration (s,zeroFrame)

nextIteration :: Iteration -> Iteration
nextIteration (Iteration(s@(Speed speed), Frame i)) = Iteration (s, Frame (i + speed))

previousIteration :: Iteration -> Iteration
previousIteration (Iteration(s@(Speed speed), Frame i)) = Iteration (s, Frame (i - speed))

zeroFrame :: Frame
zeroFrame = Frame 0
