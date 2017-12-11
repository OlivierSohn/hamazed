
module Test.Interpolation
           ( testInterpolation
           , testCoords
           , testListCoords
           , testListRenderStates
           , testInts
           , testListInts
           , testSuccessiveInts
           , testClock ) where

import Geo.Discrete
import Interpolation
import Math
import Render
import Render.Console
import Game.World.Frame

testInterpolation :: IO ()
testInterpolation = mapM_ print testClock

zipAll :: (DiscretelyInterpolable a) => Evolution a -> Frame -> (a, Maybe Float)
zipAll e x = (evolve e x, evolveDeltaTime e x)



testCoords :: [(Coords, Maybe Float)]
testCoords =
  let from = Coords (Row 0) (Col 0)
      to = Coords (Row 1) (Col 0)
      d = distance from to
      e = mkEvolution2 from to 1
  in map (zipAll e . Frame) [0..pred d]

testListCoords :: [([Coords], Maybe Float)]
testListCoords =
  let from = [Coords (Row 0) (Col 0),Coords (Row 10) (Col 10)]
      to = [Coords (Row 1) (Col 0), Coords (Row 11) (Col 10)]
      d = distance from to
      e = mkEvolution2 from to 1
  in map (zipAll e . Frame) [0..pred d]

testListRenderStates :: Context -> [([RenderState], Maybe Float)]
testListRenderStates ctxt =
  let from = [RenderState (Coords (Row 0) (Col 0)) ctxt, RenderState (Coords (Row 10) (Col 10)) ctxt]
      to   = [RenderState (Coords (Row 1) (Col 0)) ctxt, RenderState (Coords (Row 11) (Col 10)) ctxt]
      d = distance from to
      e = mkEvolution2 from to 1
  in map (zipAll e . Frame) [0..pred d]

testInts :: [(Int, Maybe Float)]
testInts =
  let from = 0
      to = 20
      d = distance from to
      e = mkEvolution2 from to 1
  in map (zipAll e . Frame) [0..pred d]

testListInts :: [([] Int, Maybe Float)]
testListInts =
  let from = [0,13]
      to = [1,11]
      d = distance from to
      e = mkEvolution2 from to 1
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
  in map (\f -> (f, evolveDeltaTime clock f)) [0..lastFrame]
