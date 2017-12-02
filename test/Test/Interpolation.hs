
module Test.Interpolation(testInterpolation) where

import GHC.Generics(Generic(..))

import Geo.Discrete
import Interpolation
import Math
import Render
import Game.World.Frame

testInterpolation :: IO ()
testInterpolation = mapM_ print testListRenderStates -- (map toEv testGlobal)

data (DiscretelyInterpolable v) => Ev v = Ev {
    _evolutionFrom :: !v
  , _evolutionTo :: !v
  , _evolutionLastFrame :: !Frame
  , _evolutionDuration :: Float -- ^ Total duration in seconds
} deriving(Show)

{--
testGlobal :: [Evolution RenderState]
testGlobal =
  let from = FrameSpec (WorldSize (Coords (Row 10) (Col 10))) (RenderState (Coords (Row 1) (Col 1)))
      to = FrameSpec (WorldSize (Coords (Row 12) (Col 12))) (RenderState (Coords (Row 0) (Col 0)))
  in createInterpolations from to 1
--}

toEv :: Evolution RenderState -> Ev RenderState
toEv (Evolution a b c d _) = Ev a b c d

testCoords :: [(Coords, Maybe Float)]
testCoords =
  let from = Coords (Row 0) (Col 0)
      to = Coords (Row 1) (Col 0)
      d = distance from to
      e = mkEvolution from to 1
  in map (evolve e . Frame) [0..d]

testListCoords :: [([Coords], Maybe Float)]
testListCoords =
  let from = [Coords (Row 0) (Col 0),Coords (Row 10) (Col 10)]
      to = [Coords (Row 1) (Col 0), Coords (Row 11) (Col 10)]
      d = distance from to
      e = mkEvolution from to 1
  in map (evolve e . Frame) [0..d]

testListRenderStates :: [([RenderState], Maybe Float)]
testListRenderStates =
  let from = [RenderState $ Coords (Row 0) (Col 0),RenderState $ Coords (Row 10) (Col 10)]
      to = [RenderState $ Coords (Row 1) (Col 0),RenderState $ Coords (Row 11) (Col 10)]
      d = distance from to
      e = mkEvolution from to 1
  in map (evolve e . Frame) [0..d]

testInts :: [(Int, Maybe Float)]
testInts =
  let from = 0
      to = 1
      d = distance from to
      e = mkEvolution from to 1
  in map (evolve e . Frame) [0..d]

testListInts :: [([] Int, Maybe Float)]
testListInts =
  let from = [0,13]
      to = [1,11]
      d = distance from to
      e = mkEvolution from to 1
  in map (evolve e . Frame) [0..d]
