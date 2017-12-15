module Test.Rendering(testSpace) where

import Control.Monad( void )

import Game.World.Space
import Render.Delta hiding(flush)

testSpace :: IO()
testSpace = do
  let blocksSize = 6
  s <- mkRandomlyFilledSpace (RandomParameters blocksSize StrictlyOneComponent) (WorldSize $ Coords 36 72)
  mkRenderFunctions <$> newDefaultContext >>= \(RenderFunctions _ renderChars _ flush) -> do
    void (renderSpace s (Coords 0 0) renderChars)
    flush
