module Test.Rendering(testSpace) where

import Control.Monad( void )

import Env
import Game.World.Space

testSpace :: ReaderT Env IO ()
testSpace = do
  let blocksSize = 6
  s <- liftIO $ mkRandomlyFilledSpace (RandomParameters blocksSize StrictlyOneComponent) (WorldSize $ Coords 36 72)
  void (renderSpace s (Coords 0 0))
  flush
