{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Graphics.Class.Colorable
            ( Colorable(..)
            -- * reexport
            , Draw, MonadReader
            ) where

import           Imj.Prelude
import           Imj.Graphics.Class.Draw

import           Control.Monad.Reader.Class(MonadReader)

-- | A 'Colorable' is a colourless graphical element.
class Colorable a where
  -- | To draw a 'Colorable', we need to pass a 'LayeredColor'.
  drawUsingColor :: (Draw e, MonadReader e m, MonadIO m)
                 => a -> LayeredColor -> m ()
