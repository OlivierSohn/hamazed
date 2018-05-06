{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Imj.Graphics.Class.UIInstructions
        ( UIInstructions(..)
        , Instructions(..)
        , ConfigUI(..)
        ) where

import           Imj.Prelude

import           Imj.Data.AlmostFloat
import           Imj.Graphics.UI.Slider
import           Imj.Graphics.Color.Types

class UIInstructions s where
  instructions :: LayeredColor
               -- ^ The color to use to draw instructions
               -> s
               -> [ConfigUI]

instance UIInstructions () where
  instructions _ _ = []

data ConfigUI = ConfigUI {
    _title :: !Text
    -- ^ Describing the parameter
  , _conf :: !Instructions
}

data Instructions =
    Choice ![Text]
  | Continuous !(Slider AlmostFloat)
  | Discrete !(Slider Int)
