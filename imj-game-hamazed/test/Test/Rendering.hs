module Test.Rendering(testSpace) where

import Control.Monad( void )
import Control.Monad.Reader(liftIO)

import Game.World.Space

testSpace :: (MonadReader e m, Draw e, MonadIO m) => m ()
testSpace = do
  let blocksSize = 6
  s <- liftIO $ mkRandomlyFilledSpace (RandomParameters blocksSize StrictlyOneComponent) (Size 36 72)
  void (renderSpace s (Coords 0 0))
