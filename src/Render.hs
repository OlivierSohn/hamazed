{-# LANGUAGE NoImplicitPrelude #-}

module Render (
          align
        , align'
        , Alignment(..)
        , renderAlignedTxt
        , renderAlignedTxt_
        , RenderFunctions(..)
        -- | reexports
        , Coords(..)
        , Row
        , Col
        , Direction(..)
        , LayeredColor(..)
        ) where

import           Imajuscule.Prelude

import           Data.Text( length )

import           Color.Types
import           Geo.Discrete


data RenderFunctions = RenderFunctions {
    _renderChar :: !(Char -> Coords -> LayeredColor -> IO ())
  , _renderChars :: !(Int -> Char -> Coords -> LayeredColor -> IO ())
  , _renderTxt :: !(Text -> Coords -> LayeredColor -> IO ())
  , _flush :: !(IO())
}

data Alignment = Centered
               | RightAligned

renderAlignedTxt_ :: (Text -> Coords -> LayeredColor -> IO ())
                  -> Alignment
                  -> Text
                  -> Coords
                  -> LayeredColor
                  -> IO ()
renderAlignedTxt_ render a txt pos colors = do
  let leftCorner = align' a (length txt) pos
  render txt leftCorner colors

renderAlignedTxt :: (Text -> Coords -> LayeredColor -> IO ())
                 -> Alignment
                 -> Text
                 -> Coords
                 -> LayeredColor
                 -> IO Coords
renderAlignedTxt render a txt pos colors =
  renderAlignedTxt_ render a txt pos colors >> return (translateInDir Down pos)

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
