
module Test.Interpolation(testInterpolation) where

import GHC.Generics(Generic(..))

import Text.Show.Functions

import Geo.Discrete
import Interpolation
import Math
import Render
import Game.World.Frame

testInterpolation :: IO ()
testInterpolation = mapM_ print testInts

zipAll :: (DiscretelyInterpolable a) => Evolution a -> Frame -> (a, Maybe Float)
zipAll e x = (evolve e x, evolveDeltaTime e x)

testCoords :: [(Coords, Maybe Float)]
testCoords =
  let from = Coords (Row 0) (Col 0)
      to = Coords (Row 1) (Col 0)
      d = distance from to
      e = mkEvolution from to 1
  in map (zipAll e . Frame) [0..pred d]

testListCoords :: [([Coords], Maybe Float)]
testListCoords =
  let from = [Coords (Row 0) (Col 0),Coords (Row 10) (Col 10)]
      to = [Coords (Row 1) (Col 0), Coords (Row 11) (Col 10)]
      d = distance from to
      e = mkEvolution from to 1
  in map (zipAll e . Frame) [0..pred d]

testListRenderStates :: [([RenderState], Maybe Float)]
testListRenderStates =
  let from = [RenderState $ Coords (Row 0) (Col 0),RenderState $ Coords (Row 10) (Col 10)]
      to = [RenderState $ Coords (Row 1) (Col 0),RenderState $ Coords (Row 11) (Col 10)]
      d = distance from to
      e = mkEvolution from to 1
  in map (zipAll e . Frame) [0..pred d]

testInts :: [(Int, Maybe Float)]
testInts =
  let from = 0
      to = 20
      d = distance from to
      e = mkEvolution from to 1
  in map (zipAll e . Frame) [0..pred d]

testListInts :: [([] Int, Maybe Float)]
testListInts =
  let from = [0,13]
      to = [1,11]
      d = distance from to
      e = mkEvolution from to 1
  in map (zipAll e . Frame) [0..pred d]
