{-# LANGUAGE NoImplicitPrelude #-}

module Game.World.Embedded
    ( mkEmbeddedWorld
    ) where

import           Imajuscule.Prelude

import           Data.String

import qualified System.Console.Terminal.Size as Terminal( Window(..), size )

import           Game.World.Size
import           Game.World.Types

import           Render
import           Render.Console

-- | Minimal margin between the upper left corner of the console
--   and upper left corner of the world
minimalWorldMargin :: Int
minimalWorldMargin = 4


mkEmbeddedWorld :: Context -> WorldSize -> IO (Either String EmbeddedWorld)
mkEmbeddedWorld ctxt s = do
  mayTermSize <- Terminal.size
  return $ (EmbeddedWorld mayTermSize . flip RenderState ctxt) <$> worldUpperLeftToCenterIt' s mayTermSize


worldUpperLeftToCenterIt' :: WorldSize -> Maybe (Terminal.Window Int) -> Either String Coords
worldUpperLeftToCenterIt' worldSize mayTermSize =
  case mayTermSize of
    Just termSize@(Terminal.Window h w)  ->
      let (WorldSize (Coords (Row rs) (Col cs))) = maxWorldSize
          heightMargin = 2 * (1 {-outer walls-} + 1 {-1 line above and below-})
          widthMargin = 2 * (1 {-outer walls-} + 4 {-brackets, spaces-} + 16 * 2 {-display all numbers-})
          minSize@(Terminal.Window minh minw) = Terminal.Window (rs + heightMargin) (cs + widthMargin)
      in if h < minh || w < minw
            then
              Left $  "\nMinimum terminal size : " ++ show minSize
                  ++ ".\nCurrent terminal size : " ++ show termSize
                  ++ ".\nThe current terminal size doesn't match the minimum size,"
                  ++  "\nplease adjust your terminal size and restart the executable"
                  ++ ".\n"
            else
              Right $ worldUpperLeftFromTermSize termSize worldSize
    Nothing -> Right $ Coords (Row minimalWorldMargin) (Col minimalWorldMargin)

worldUpperLeftFromTermSize :: Terminal.Window Int -> WorldSize -> Coords
worldUpperLeftFromTermSize (Terminal.Window h w) (WorldSize (Coords (Row rs) (Col cs))) =
  let walls = 2 :: Int
  in Coords (Row $ quot (h-(rs+walls)) 2) (Col $ quot (w-(cs+walls)) 2)
