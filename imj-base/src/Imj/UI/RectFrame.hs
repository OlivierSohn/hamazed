{-# LANGUAGE NoImplicitPrelude #-}

module Imj.UI.RectFrame
        ( RectFrame(..)
        ) where

import           Imj.Prelude

import           Imj.Geo.Discrete.Types
import           Imj.Color.Types

data RectFrame = RectFrame {
    _rectFrameSize :: !Size
  , _rectFrameUpperLeft :: !Coords
  , _rectFrameColors :: !LayeredColor
} deriving(Eq, Show)
