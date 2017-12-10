module Test.Rendering(testSpace) where

import Game.World.Space
import Game.World.Size
import Render.Console

testSpace :: IO()
testSpace = do
  let blocksSize = 6
      ws = worldSizeFromLevel 1 Rectangle2x1
  s <- mkRandomlyFilledSpace (RandomParameters blocksSize StrictlyOneComponent) (WorldSize $ Coords (Row 36) (Col 72))
  setFrameDimensions TerminalSize
  beginFrame
  renderSpace s $ RenderState (Coords (Row 0) (Col 0))
  endFrame
  return ()
