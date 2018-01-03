{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Graphics.Class.HasRectArea
  ( HasRectArea(..)
  -- * reexport
  , RectArea
  ) where

import           Imj.Graphics.UI.RectArea

class HasRectArea e where
  getRectArea :: e -> RectArea a

instance HasRectArea (RectArea a) where
  {-# INLINE getRectArea #-}
  getRectArea (RectArea ul lr) = RectArea ul lr
