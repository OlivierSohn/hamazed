{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Graphics.Class.HasLayeredColor
            ( HasLayeredColor(..)
            -- * reexport
            , LayeredColor
            ) where

import           Imj.Graphics.Color.Types

-- | Access one graphical element's 'LayeredColor'.
class HasLayeredColor a where
  getColor :: a -> LayeredColor
