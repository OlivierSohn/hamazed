{-# OPTIONS_HADDOCK hide #-} -- TODO refactor and doc

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Imj.Graphics.UI.Animation.Types
           ( UIEvolutions(..)
           , UIAnimation(..)
           , UIAnimProgress(..)
           , mkZeroAnimation
           ) where

import           Imj.Prelude

import           Imj.Graphics.Class.Positionable
import           Imj.Graphics.Interpolation.Evolution
import           Imj.Graphics.Text.Animation
import           Imj.Graphics.Text.Animation.Types
import           Imj.Graphics.Text.ColoredGlyphList
import           Imj.Graphics.UI.Colored
import           Imj.Graphics.UI.RectContainer
import           Imj.Timing

-- | Manages the progress and deadline of 'UIEvolutions'.
data UIAnimation = UIAnimation {
    _uiAnimationEvs :: {-# UNPACK #-} !UIEvolutions
  , getProgress :: !UIAnimProgress
  -- ^ Current 'Iteration'.
} deriving(Generic)
instance PrettyVal UIAnimation where
  prettyVal (UIAnimation a b) = prettyVal ("UIAnimation",a,b)
instance HasReferencePosition UIAnimation where
  getPosition (UIAnimation a _) = getPosition a
  {-# INLINE getPosition #-}
instance Show UIAnimation where
  show (UIAnimation a b) = show ("UIAnimation",a,b)
instance GeoTransform UIAnimation where
  transform f (UIAnimation a b) = UIAnimation (transform f a) b
  {-# INLINE transform #-}

mkZeroAnimation :: UIAnimation
mkZeroAnimation = UIAnimation mkZeroEvolutions $ UIAnimProgress Nothing $ zeroIteration 1

data UIAnimProgress = UIAnimProgress {
    _deadline :: {-unpack sum-} !(Maybe (Time Point System))
  -- ^ Time at which the 'UIEvolutions' should be rendered and updated
  , _progress :: {-# UNPACK #-} !Iteration
  -- ^ Current 'Iteration'.
} deriving(Show, Generic, PrettyVal)

-- TODO generalize as an Evolution (text-decorated RectContainer)
-- | Used when transitionning between two levels to smoothly transform the aspect
-- of the 'RectContainer', as well as textual information around it.
data UIEvolutions = UIEvolutions {
    _uiEvolutionContainer :: {-# UNPACK #-} !(Evolution (Colored RectContainer))
    -- ^ The transformation of the 'RectContainer'.
  , _uiEvolutionsUpDown :: {-# UNPACK #-} !(TextAnimation ColoredGlyphList AnchorChars)
    -- ^ The transformation of colored text at the top and at the bottom of the 'RectContainer'.
  , _uiEvolutionLeft    :: {-# UNPACK #-} !(TextAnimation ColoredGlyphList AnchorStrings)
    -- ^ The transformation of colored text left and right of the 'RectContainer'.
} deriving(Show, PrettyVal, Generic)
instance HasReferencePosition UIEvolutions where
  getPosition (UIEvolutions container _ _) = getPosition container
instance GeoTransform UIEvolutions where
  transform f (UIEvolutions a b c) = UIEvolutions (transform f a) (transform f b) (transform f c)

mkZeroEvolutions :: UIEvolutions
mkZeroEvolutions = UIEvolutions mkEmptyEvolution mkEmptyTextAnimation mkEmptyTextAnimation
