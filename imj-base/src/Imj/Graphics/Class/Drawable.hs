{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Graphics.Class.Drawable
            ( Drawable(..)
            -- * reexport
            , Draw, MonadReader
            ) where

import           Imj.Prelude
import           Control.Monad.Reader.Class(MonadReader)

import           Imj.Graphics.Class.Draw

-- | A 'Drawable' is a graphical element that knows how to draw itself
-- (it knows its color and position).
class Drawable a where
  -- | Draw the 'Drawable'
  draw :: (Draw e, MonadReader e m, MonadIO m)
       => a -> m ()
