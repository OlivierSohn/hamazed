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
import           Prelude(length)
import           Control.Monad.Reader.Class(MonadReader)

import qualified Data.Text as Text
import           Imj.Data.AlmostFloat
import           Imj.Graphics.Class.Draw
import           Imj.Graphics.Class.Positionable
import           Imj.Geo.Discrete.Types
import           Imj.Graphics.UI.Colored
import           Imj.Graphics.UI.Slider
import qualified Imj.Graphics.UI.Choice as UI

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
  , _conf :: ![Instructions]
}

data Instructions =
    List !LayeredColor ![Text]
  | Choice !UI.Choice
  | Continuous !(Slider AlmostFloat)
  | Discrete !(Slider Int)
instance Positionable Instructions where
  drawAt o pos = case o of
    List color l -> void $ drawList color l pos
    Choice c -> drawAt c pos
    Continuous s -> drawAt s pos
    Discrete s -> drawAt s pos

  width = \case
    List _ l -> fromIntegral $ fromMaybe 0 $ maximumMaybe $ map Text.length l
    Choice c -> width c
    Continuous s -> width s
    Discrete s -> width s

  height = \case
    List _ l -> fromIntegral $ length l
    Choice c -> height c
    Continuous s -> height s
    Discrete s -> height s

{-# INLINABLE drawList #-}
drawList :: (MonadIO m, MonadReader e m, Draw e, Foldable t)
         => LayeredColor -> t Text -> Coords Pos -> m (Coords Pos)
drawList color s al =
  foldM (flip (dText color)) al s

dText :: (Draw e, MonadReader e m, MonadIO m)
        => LayeredColor -> Text -> Coords Pos -> m (Coords Pos)
dText color txt pos = do
  drawAt (Colored color txt) pos
  return $ translateInDir Down pos
