{-# LANGUAGE NoImplicitPrelude #-}

module Render (
          align
        , renderAligned
        , renderColored
        , renderColoredChars
        , drawChar
        , renderPoints
        , Alignment(..)
        , renderAlignedTxt
        , renderAlignedTxt_
        , ColorString(..)
        , colored
        -- | reexports
        , Coords(..)
        , Row(..)
        , Col(..)
        , Direction(..)
        , Color8Code(..)
        , ConsoleLayer(..)
        , Colors(..)
        , IORef
        , Buffers
        ) where

import           Imajuscule.Prelude

import           Control.Monad( foldM_ )

import           Data.Text( length )

import           Geo.Discrete.Types
import           Geo.Discrete( move, sumCoords, translateInDir )

import           Render.Console

import           Text.ColorString


drawChar :: Char -> Coords -> Colors -> IORef Buffers -> IO ()
drawChar char pos colors =
  renderChar_ char colors pos

renderPoints :: Colors -> Coords -> IORef Buffers -> [(Coords, Char)] -> IO ()
renderPoints colors pos b =
  mapM_ (\(c,char) -> drawChar char (sumCoords pos c) colors b)

renderColoredChars :: Int -> Char -> Colors -> Coords -> IORef Buffers -> IO ()
renderColoredChars =
  drawChars

data Alignment = Centered
               | RightAligned

renderAlignedTxt_ :: Alignment -> Text -> Colors -> Coords -> IORef Buffers -> IO ()
renderAlignedTxt_ a txt colors pos = do
  let leftCorner = align' a (length txt) pos
  renderTxt_ txt colors leftCorner

renderAlignedTxt :: Alignment -> Text -> Colors -> Coords -> IORef Buffers -> IO Coords
renderAlignedTxt a txt colors pos b =
  renderAlignedTxt_ a txt colors pos b >> return (translateInDir Down pos)

renderAligned :: Alignment -> ColorString -> Coords -> IORef Buffers -> IO Coords
renderAligned a cs pos b = do
  let leftCorner = align' a (countChars cs) pos
  _ <- renderColored cs leftCorner b
  return (translateInDir Down pos)

renderColored :: ColorString
              -> Coords
              -> IORef Buffers
              -> IO ()
renderColored (ColorString cs) pos b =
  foldM_ (\count (txt, color) -> do
    let l = length txt
    renderTxt_ txt color (move count RIGHT pos) b
    return $ count + l) 0 cs

align' :: Alignment -> Int -> Coords -> Coords
align' a count ref =
  let (amount, dir) = align a count
  in move amount dir ref

align :: Alignment -> Int -> (Int, Direction)
align a count =
  let amount =
        case a of
          Centered     -> 1 + quot count 2
          RightAligned -> count
  in (amount, LEFT)
