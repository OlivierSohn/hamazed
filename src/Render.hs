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

import           Data.Text( length )

import           Geo.Discrete.Types
import           Geo.Discrete( move, sumCoords, diffCoords, translateInDir )

import           Render.Console

import           Text.ColorString

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

drawChar :: Char -> Coords -> RenderState -> IO ()
drawChar char pos (RenderState upperLeftCoords) =
  renderChar_ char $ RenderState $ sumCoords pos upperLeftCoords

renderPoints :: RenderState -> [(Coords, Char)] -> IO ()
renderPoints state =
  mapM_ (\(c,char) -> drawChar char c state)

renderColoredPoints :: [(Coords, Char)] -> Color8Code -> RenderState -> IO ()
renderColoredPoints points colorCode state = do
  c <- setDrawColor Foreground colorCode
  renderPoints state points
  restoreDrawColors c

renderColoredChars :: Int -> Char -> Colors -> RenderState -> IO ()
renderColoredChars count char colors state = do
  c <- setDrawColors colors
  drawChars count char state
  restoreDrawColors c

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
renderColored (ColorString cs) ref =
  foldM_ (\count (txt, color) -> do
    let l = length txt
    c <- setDrawColor Foreground color
    renderTxt_ txt $ Render.move count RIGHT ref
    restoreDrawColors c
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
