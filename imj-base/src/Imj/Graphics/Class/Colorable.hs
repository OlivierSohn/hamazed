{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Graphics.Class.Colorable
            ( Colorable(..)
            ) where


import           Control.Monad.IO.Class(MonadIO)
import           Control.Monad.Reader.Class(MonadReader)

import           Imj.Graphics.Class.Draw
import           Imj.Graphics.Color.Types

-- | A 'Colorable' is a colourless graphical element.
class Colorable a where-- TODO add HasPosition constraint here
  -- | To draw a 'Colorable', we need to pass a 'LayeredColor'.
  drawUsingColor :: (Draw e, MonadReader e m, MonadIO m)
                 => a -> LayeredColor -> m ()
