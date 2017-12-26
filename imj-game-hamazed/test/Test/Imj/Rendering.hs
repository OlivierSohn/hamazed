module Test.Imj.Rendering(testSpace) where

import Control.Monad( void )
import Control.Monad.Reader(liftIO)

import Imj.Game.World.Space
import Imj.Geo.Discrete.Types

testSpace :: (MonadReader e m, Draw e, MonadIO m) => m ()
testSpace = do
  let blocksSize = 6
  s <- liftIO $ mkRandomlyFilledSpace (RandomParameters blocksSize StrictlyOneComponent) (Size 36 72)
  void (renderSpace s (Coords 0 0))
