{-# LANGUAGE NoImplicitPrelude #-}

module Render (
          align
        , renderAligned
        , renderColored
        , renderColoredPoints
        , renderColoredChars
        , drawChar
        , renderPoints
        , Alignment(..)
        , renderAlignedTxt
        , renderAlignedTxt_
        , go
        , Render.move
        , translate
        , translate'
        , ColorString(..)
        , colored
        , setColor
        , setColors
        -- | reexports
        , Coords(..)
        , Row(..)
        , Col(..)
        , Direction(..)
        , RenderState(..)
        , Color8Code(..)
        , ConsoleLayer(..)
        , Colors(..)
        ) where

import           Imajuscule.Prelude

import           Control.Monad( foldM_ )

import           Data.Text( length )

import           Geo.Discrete.Types
import           Geo.Discrete( move, sumCoords, translateInDir )

import           Render.Console

import           Text.ColorString

--------------------------------------------------------------------------------
-- Pure
--------------------------------------------------------------------------------

go :: Direction -> RenderState -> RenderState
go dir (RenderState ctxt color pos) = RenderState ctxt color (translateInDir dir pos)

move :: Int -> Direction -> RenderState -> RenderState
move n dir (RenderState ctxt color pos) = RenderState ctxt color (Geo.Discrete.move n dir pos)

translate :: Row -> Col -> RenderState -> RenderState
translate r c = translate' (Coords r c)

translate' :: Coords -> RenderState -> RenderState
translate' pos' (RenderState ctxt color pos) = RenderState ctxt color (sumCoords pos pos')

--------------------------------------------------------------------------------
-- IO
--------------------------------------------------------------------------------

drawChar :: Char -> Coords -> RenderState -> IO ()
drawChar char pos (RenderState ctxt colors upperLeftCoords) =
  renderChar_ char $ RenderState ctxt colors (sumCoords pos upperLeftCoords)

renderPoints :: RenderState -> [(Coords, Char)] -> IO ()
renderPoints state =
  mapM_ (\(c,char) -> drawChar char c state)

renderColoredPoints :: [(Coords, Char)] -> Color8Code -> RenderState -> IO ()
renderColoredPoints points code s =
  renderPoints (setColor Foreground code s) points

setColor :: ConsoleLayer -> Color8Code -> RenderState -> RenderState
setColor = setDrawColor

setColors :: Colors -> RenderState -> RenderState
setColors = setDrawColors

renderColoredChars :: Int -> Char -> Colors -> RenderState -> IO ()
renderColoredChars count char colors s =
  drawChars count char $ setColors colors s

data Alignment = Centered
               | RightAligned

renderAlignedTxt_ :: Alignment -> Text -> RenderState -> IO ()
renderAlignedTxt_ a txt ref = do
  let leftCorner = align' a (length txt) ref
  renderTxt_ txt leftCorner

renderAlignedTxt :: Alignment -> Text -> RenderState -> IO RenderState
renderAlignedTxt a txt ref =
  renderAlignedTxt_ a txt ref >> return (go Down ref)

renderAligned :: Alignment -> ColorString -> RenderState -> IO RenderState
renderAligned a cs ref = do
  let leftCorner = align' a (countChars cs) ref
  _ <- renderColored cs leftCorner
  return (go Down ref)

renderColored :: ColorString -> RenderState -> IO ()
renderColored (ColorString cs) s =
  foldM_ (\count (txt, color) -> do
    let l = length txt
    renderTxt_ txt (Render.move count RIGHT $ setColor Foreground color s)
    return $ count + l) 0 cs

align' :: Alignment -> Int -> RenderState -> RenderState
align' a count ref =
  let (amount, dir) = align a count
  in Render.move amount dir ref

align :: Alignment -> Int -> (Int, Direction)
align a count =
  let amount =
        case a of
          Centered     -> 1 + quot count 2
          RightAligned -> count
  in (amount, LEFT)
