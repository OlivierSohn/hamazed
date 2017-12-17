module Test.Rendering(testSpace) where

import Control.Monad( void )
import Control.Monad.Reader(liftIO, ReaderT)

import Env
import Game.World.Space

testSpace :: ReaderT Env IO ()
testSpace = do
  let blocksSize = 6
  s <- liftIO $ mkRandomlyFilledSpace (RandomParameters blocksSize StrictlyOneComponent) (WorldSize $ Coords 36 72)
  void (renderSpace s (Coords 0 0))
