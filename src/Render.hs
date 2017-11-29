{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Render (
          renderAligned
        , renderColored
        , renderColoredPoints
        , renderColoredChars
        , renderChar
        , renderPoints
        , Alignment(..)
        , renderAlignedTxt
        , renderAlignedTxt_
        , go
        , Render.move
        , translate
        , mkEmbeddedWorld
        , EmbeddedWorld(..)
        , ColorString(..)
        , colored
        -- | reexports
        , Coords(..)
        , Row(..)
        , Col(..)
        , Direction(..)
        , RenderState(..)
        , Color8Code(..)
        , Terminal.Window(..)
        ) where

import           Imajuscule.Prelude

import           Control.Monad( foldM_ )

import           Data.List( foldl' )
import           Data.Text( Text, length )
import           Data.String( String )

import qualified System.Console.Terminal.Size as Terminal( size
                                                         , Window(..))

import           Console
import           Geo.Types
import           Geo( move
                    , sumCoords
                    , translateInDir )
import           WorldSize( WorldSize(..), maxWorldSize )


data EmbeddedWorld = EmbeddedWorld {
    _embeddedWorldTerminal :: !(Maybe (Terminal.Window Int))
  , _embeddedWorldUpperLeft :: !RenderState
}


newtype ColorString = ColorString [(Text, Color8Code)]

colored :: Text -> Color8Code -> ColorString
colored t c = ColorString [(t,c)]

countChars :: ColorString -> Int
countChars (ColorString cs) = foldl' (\c (txt, _) -> c + length txt) 0 cs

instance Monoid ColorString where
  mempty = ColorString [("", Color8Code 0)]
  mappend (ColorString x) (ColorString y) = ColorString $ x ++ y

--------------------------------------------------------------------------------
-- Pure
--------------------------------------------------------------------------------

-- | Minimal margin between the upper left corner of the console
--   and upper left corner of the world
minimalWorldMargin :: Int
minimalWorldMargin = 4

go :: Direction -> RenderState -> RenderState
go dir (RenderState r) = RenderState $ translateInDir dir r

move :: Int -> Direction -> RenderState -> RenderState
move n dir (RenderState c) = RenderState $ Geo.move n dir c

translate :: Row -> Col -> RenderState -> RenderState
translate r c (RenderState coords) = RenderState $ sumCoords coords $ Coords r c

worldUpperLeftToCenterIt' :: WorldSize -> Maybe (Terminal.Window Int) -> Either String Coords
worldUpperLeftToCenterIt' worldSize mayTermSize =
  case mayTermSize of
    Just termSize@(Terminal.Window h w)  ->
      let (WorldSize (Coords (Row rs) (Col cs))) = maxWorldSize
          heightMargin = 2 * (1 {-outer walls-} + 2 {-2 lines above and below-})
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

--------------------------------------------------------------------------------
-- IO
--------------------------------------------------------------------------------

renderChar :: Char -> Coords -> RenderState -> IO ()
renderChar char pos (RenderState upperLeftCoords) =
  renderChar_ char $ RenderState $ sumCoords pos upperLeftCoords

renderPoints :: RenderState -> [(Coords, Char)] -> IO ()
renderPoints state =
  mapM_ (\(c,char) -> renderChar char c state)

renderColoredPoints :: [(Coords, Char)] -> Color8Code -> RenderState -> IO ()
renderColoredPoints points colorCode state = do
  fg <- setRawForeground colorCode
  renderPoints state points
  restoreForeground fg

renderColoredChars :: Int -> Char -> (Color8Code, Color8Code) -> RenderState -> IO ()
renderColoredChars count char colors state = do
  c <- setColors colors
  renderChars count char state
  restoreColors c

data Alignment = Centered
               | RightAligned

renderAlignedTxt_ :: Alignment -> Text -> RenderState -> IO ()
renderAlignedTxt_ a txt ref = do
  let leftCorner = align a (length txt) ref
  renderTxt_ txt leftCorner

renderAlignedTxt :: Alignment -> Text -> RenderState -> IO RenderState
renderAlignedTxt a txt ref =
  renderAlignedTxt_ a txt ref >> return (go Down ref)

renderAligned :: Alignment -> ColorString -> RenderState -> IO RenderState
renderAligned a cs ref = do
  let leftCorner = align a (countChars cs) ref
  _ <- renderColored cs leftCorner
  return (go Down ref)

renderColored :: ColorString -> RenderState -> IO RenderState
renderColored (ColorString cs) ref = do
  foldM_ (\count (txt, color) -> do
    let l = length txt
    fg <- setRawForeground color
    renderTxt_ txt $ Render.move count RIGHT ref
    restoreForeground fg
    return $ count + l) 0 cs
  return (go Down ref)

align :: Alignment -> Int -> RenderState -> RenderState
align a count ref =
  let amount = case a of
        Centered     -> 1 + quot count 2
        RightAligned -> count
  in Render.move amount LEFT ref

mkEmbeddedWorld :: WorldSize -> IO (Either String EmbeddedWorld)
mkEmbeddedWorld s = do
  mayTermSize <- Terminal.size
  return $ (EmbeddedWorld mayTermSize . RenderState) <$> worldUpperLeftToCenterIt' s mayTermSize
