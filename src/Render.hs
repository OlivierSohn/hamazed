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
        , sumRS
        , diffRS
        , ColorString(..)
        , colored
        -- | reexports
        , Coords(..)
        , Row(..)
        , Col(..)
        , Direction(..)
        , RenderState(..)
        , Color8Code(..)
        ) where

import           Imajuscule.Prelude

import           Control.Monad( foldM_ )

import           Data.List( foldl' )
import           Data.Text( Text, length )

import           Geo.Discrete.Types
import           Geo.Discrete( move, sumCoords, diffCoords, translateInDir )

import           Render.Console



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

go :: Direction -> RenderState -> RenderState
go dir (RenderState r) = RenderState $ translateInDir dir r

move :: Int -> Direction -> RenderState -> RenderState
move n dir (RenderState c) = RenderState $ Geo.Discrete.move n dir c

translate :: Row -> Col -> RenderState -> RenderState
translate r c (RenderState coords) = RenderState $ sumCoords coords $ Coords r c

sumRS :: RenderState -> RenderState -> RenderState
sumRS (RenderState c1) (RenderState c2) = RenderState $ sumCoords c1 c2

diffRS :: RenderState -> RenderState -> RenderState
diffRS (RenderState c1) (RenderState c2) = RenderState $ diffCoords c1 c2

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
