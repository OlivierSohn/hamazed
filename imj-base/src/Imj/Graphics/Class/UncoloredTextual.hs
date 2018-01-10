{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}

module Imj.Graphics.Class.UncoloredTextual
            ( UncoloredTextual(..)
            ) where

import           Imj.Prelude
import qualified Prelude(length)

import qualified Data.Text as Text(length)

import           Imj.Geo.Discrete.Types
import           Imj.Graphics.Text.ColorString
import           Imj.Graphics.Render.FromMonadReader

-- | A 'UncoloredTextual' is a textual type that contains no color information.
class UncoloredTextual a where
  drawTextual :: (Draw e, MonadReader e m, MonadIO m)
              => a -> Coords Pos -> LayeredColor -> m ()
  textLength :: a -> Length Width

instance UncoloredTextual Char where
  drawTextual = drawChar
  {-# INLINABLE drawTextual #-}
  textLength _ = 1
  {-# INLINABLE textLength #-}
instance UncoloredTextual ([] Char) where
  drawTextual = drawStr
  {-# INLINABLE drawTextual #-}
  textLength = fromIntegral . Prelude.length
  {-# INLINABLE textLength #-}
instance UncoloredTextual Text where
  drawTextual = drawTxt
  {-# INLINABLE drawTextual #-}
  textLength = fromIntegral . Text.length
  {-# INLINABLE textLength #-}
