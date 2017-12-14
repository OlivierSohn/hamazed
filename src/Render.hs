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

import           Data.Text( length )

import           Geo.Discrete.Types
import           Geo.Discrete( move, sumCoords, translateInDir )

import           Render.Console

import           Text.ColorString


renderPoints :: IORef Buffers -> LayeredColor -> Coords -> [(Coords, Char)] -> IO ()
renderPoints ref colors pos =
  mapM_ (\(c,char) -> drawChar ref char (sumCoords pos c) colors)

renderColoredChars :: IORef Buffers -> Int -> Char -> Coords -> LayeredColor -> IO ()
renderColoredChars =
  drawChars

data Alignment = Centered
               | RightAligned

renderAlignedTxt_ :: IORef Buffers -> Alignment -> Text -> Coords -> LayeredColor -> IO ()
renderAlignedTxt_ ref a txt pos colors = do
  let leftCorner = align' a (length txt) pos
  drawTxt_ ref txt leftCorner colors

renderAlignedTxt :: IORef Buffers -> Alignment -> Text -> Coords -> LayeredColor -> IO Coords
renderAlignedTxt ref a txt pos colors =
  renderAlignedTxt_ ref a txt pos colors >> return (translateInDir Down pos)

renderAligned :: Alignment
              -> ColorString
              -> Coords
              -> (Text -> Coords -> LayeredColor -> IO ())
              -> IO Coords
renderAligned a cs pos r = do
  let leftCorner = align' a (countChars cs) pos
  _ <- renderColored cs leftCorner r
  return (translateInDir Down pos)

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
