{-# LANGUAGE NoImplicitPrelude #-}

module Game.World.Embedded
    ( mkEmbeddedWorld
    ) where

import           Imajuscule.Prelude

import           Data.String

import qualified System.Console.Terminal.Size as Terminal( Window(..), size )

import           Game.World.Size
import           Game.World.Types

-- | Minimal margin between the upper left corner of the console
--   and upper left corner of the world
minimalWorldMargin :: Int
minimalWorldMargin = 4


mkEmbeddedWorld :: WorldSize -> IO (Either String EmbeddedWorld)
mkEmbeddedWorld s = do
  mayTermSize <- Terminal.size
  return $ EmbeddedWorld mayTermSize <$> worldUpperLeftToCenterIt' s mayTermSize


worldUpperLeftToCenterIt' :: WorldSize -> Maybe (Terminal.Window Int) -> Either String Coords
worldUpperLeftToCenterIt' worldSize mayTermSize =
  case mayTermSize of
    Just termSize@(Terminal.Window h w)  ->
      let (WorldSize (Coords rs cs)) = maxWorldSize
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

worldUpperLeftFromTermSize :: Terminal.Window Int -> WorldSize -> Coords
worldUpperLeftFromTermSize (Terminal.Window h w) (WorldSize (Coords rs cs)) =
  let walls = 2 :: Int
  in Coords (quot (fromIntegral h-(rs+ fromIntegral walls)) 2)
            (quot (fromIntegral w-(cs+ fromIntegral walls)) 2)
