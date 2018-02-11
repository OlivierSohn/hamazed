module Test.Imj.Render(testSpace) where

import Control.Monad( void )
import Control.Monad.Reader(liftIO)

import Imj.Game.Hamazed.World.Space
import Imj.Geo.Discrete.Types

testSpace :: (MonadReader e m, Draw e, MonadIO m) => m ()
testSpace = do
  let blocksSize = 6
  s <- liftIO $ mkRandomlyFilledSpace (RandomParameters blocksSize StrictlyOneComponent) (Size 36 72)
  void (drawSpace (mkRenderedSpace s) (Coords 0 0))
