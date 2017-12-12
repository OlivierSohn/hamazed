{-# LANGUAGE NoImplicitPrelude #-}

module Render (
          align
        , renderAligned
        , renderColored
        , renderColoredChars
        , renderPoints
        , Alignment(..)
        , renderAlignedTxt
        , renderAlignedTxt_
        , ColorString(..)
        , colored
        -- | reexports
        , Coords(..)
        , Row
        , Col
        , Direction(..)
        , Color8Code(..)
        , ConsoleLayer(..)
        , LayeredColor(..)
        , IORef
        , Buffers
        , drawChar
        ) where

import           Imajuscule.Prelude

import           Control.Monad( foldM_ )

import           Data.Text( length )

import           Geo.Discrete.Types
import           Geo.Discrete( move, sumCoords, translateInDir )

import           Render.Console

import           Text.ColorString


renderPoints :: LayeredColor -> Coords -> IORef Buffers -> [(Coords, Char)] -> IO ()
renderPoints colors pos b =
  mapM_ (\(c,char) -> drawChar char (sumCoords pos c) colors b)

renderColoredChars :: Int -> Char -> Coords -> LayeredColor -> IORef Buffers -> IO (IORef Buffers)
renderColoredChars =
  drawChars

data Alignment = Centered
               | RightAligned

renderAlignedTxt_ :: Alignment -> Text -> Coords -> LayeredColor -> IORef Buffers -> IO ()
renderAlignedTxt_ a txt pos colors = do
  let leftCorner = align' a (length txt) pos
  drawTxt_ txt leftCorner colors

renderAlignedTxt :: Alignment -> Text -> Coords -> LayeredColor -> IORef Buffers -> IO Coords
renderAlignedTxt a txt pos colors b =
  renderAlignedTxt_ a txt pos colors b >> return (translateInDir Down pos)

renderAligned :: Alignment -> ColorString -> Coords -> IORef Buffers -> IO Coords
renderAligned a cs pos b = do
  let leftCorner = align' a (countChars cs) pos
  _ <- renderColored cs leftCorner b
  return (translateInDir Down pos)

renderColored :: ColorString
              -> Coords
              -> IORef Buffers
              -> IO (IORef Buffers)
renderColored (ColorString cs) pos b = do
  foldM_ (\count (txt, color) -> do
    let l = length txt
    _ <- drawTxt_ txt (move count RIGHT pos) color b
    return $ count + l) 0 cs
  return b

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
