
module Test.Interpolation
           ( testInterpolation
           , testCoords
           , testListCoords
           , testInts
           , testListInts
           , testSuccessiveInts
           , testClock ) where

import Geo.Discrete
import Interpolation
import Math
import Game.World.Types

testInterpolation :: IO ()
testInterpolation = mapM_ print testClock

zipAll :: (DiscretelyInterpolable a) => Evolution a -> Frame -> (a, Maybe Float)
zipAll e x = (getValueAt e x, getDeltaTimeToNextFrame e x)



testCoords :: [(Coords, Maybe Float)]
testCoords =
  let from = Coords 0 0
      to = Coords 1 0
      d = distance from to
      e = mkEvolution (Successive [from, to]) 1
  in map (zipAll e . Frame) [0..pred d]

testListCoords :: [([Coords], Maybe Float)]
testListCoords =
  let from = [Coords 0 0, Coords 10 10]
      to   = [Coords 1 0, Coords 11 10]
      d = distance from to
      e = mkEvolution (Successive [from, to]) 1
  in map (zipAll e . Frame) [0..pred d]

testInts :: [(Int, Maybe Float)]
testInts =
  let from = 0
      to = 20
      d = distance from to
      e = mkEvolution (Successive [from, to]) 1
  in map (zipAll e . Frame) [0..pred d]

testListInts :: [([] Int, Maybe Float)]
testListInts =
  let from = [0,13]
      to = [1,11]
      d = distance from to
      e = mkEvolution (Successive [from, to]) 1
  in map (zipAll e . Frame) [0..pred d]

testSuccessiveInts :: [(Int, Maybe Float)]
testSuccessiveInts =
  let s = Successive [3,7,9,5]
      e = mkEvolution s 1
      d = distanceSuccessive s
  in map (zipAll e . Frame) [0..pred d]

testClock :: [(Frame, Maybe Float)]
testClock =
  let lastFrame = Frame 10
      (EaseClock clock) = mkEaseClock 1 lastFrame invQuartEaseInOut
  in map (\f -> (f, getDeltaTimeToNextFrame clock f)) [0..lastFrame]
