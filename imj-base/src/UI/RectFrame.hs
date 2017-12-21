{-# LANGUAGE NoImplicitPrelude #-}

module UI.RectFrame
        ( RectFrame(..)
        ) where

import           Imajuscule.Prelude

import           Geo.Discrete.Types
import           Color.Types

data RectFrame = RectFrame {
    _rectFrameSize :: !Size
  , _rectFrameUpperLeft :: !Coords
  , _rectFrameColors :: !LayeredColor
} deriving(Eq, Show)
