{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Game.World.Embedded
    ( mkEmbeddedWorld
    ) where

import           Imj.Prelude

import           Control.Monad.IO.Class(MonadIO)

import qualified System.Console.Terminal.Size as Terminal( Window(..), size )

import           Imj.Game.World.Size
import           Imj.Game.World.Types
import           Imj.Geo.Discrete.Types

-- | Minimal margin between the upper left corner of the console
--   and upper left corner of the world
minimalWorldMargin :: Int
minimalWorldMargin = 4

-- | Will compute the position of the 'World' so as to display it in the
-- center of the terminal window.
{-# INLINABLE mkEmbeddedWorld #-}
mkEmbeddedWorld :: (MonadIO m)
                => Size
                -> m (Either String EmbeddedWorld)
mkEmbeddedWorld s = do
  mayTermSize <- liftIO Terminal.size
  return $ EmbeddedWorld mayTermSize <$> worldUpperLeftToCenterIt' s mayTermSize


worldUpperLeftToCenterIt' :: Size -> Maybe (Terminal.Window Int) -> Either String Coords
worldUpperLeftToCenterIt' worldSize mayTermSize =
  case mayTermSize of
    Just termSize@(Terminal.Window h w)  ->
      let (Size rs cs) = maxWorldSize
          heightMargin = 2 * (1 {-outer walls-} + 1 {-1 line above and below-})
          widthMargin = 2 * (1 {-outer walls-} + 4 {-brackets, spaces-} + 16 * 2 {-display all numbers-})
          minSize@(Terminal.Window minh minw) =
            Terminal.Window (fromIntegral rs + heightMargin)
                            (fromIntegral cs + widthMargin)
      in if h < minh || w < minw
            then
              Left $  "\nMinimum terminal size : " ++ show minSize
                  ++ ".\nCurrent terminal size : " ++ show termSize
                  ++ ".\nThe current terminal size doesn't match the minimum size,"
                  ++  "\nplease adjust your terminal size and restart the executable"
                  ++ ".\n"
            else
              Right $ worldUpperLeftFromTermSize termSize worldSize
    Nothing -> Right $ Coords (Coord minimalWorldMargin) (Coord minimalWorldMargin)

worldUpperLeftFromTermSize :: Terminal.Window Int -> Size -> Coords
worldUpperLeftFromTermSize (Terminal.Window h w) (Size rs cs) =
  let walls = 2 :: Int
  in toCoords (quot (fromIntegral h-(rs+ fromIntegral walls)) 2)
              (quot (fromIntegral w-(cs+ fromIntegral walls)) 2)
