{-# OPTIONS_HADDOCK prune #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Imj.Graphics.Text.Animation.Types
          ( TextAnimation(..)
          , mkEmptyTextAnimation
          , AnchorChars
          , AnchorStrings
          ) where

import           Imj.Prelude

import           Imj.Geo.Discrete.Types
import           Imj.Graphics.Class.Positionable
import           Imj.Graphics.Interpolation
import           Imj.Timing

-- | One anchor per String
data AnchorStrings
-- | One anchor per Character
data AnchorChars


-- | Interpolates 'ColorString's and anchors.
data TextAnimation b a = TextAnimation {
   _textAnimationEvolutions :: ![Evolution b]
 , _textAnimationAnchors :: {-# UNPACK #-} !(Evolution (SequentiallyInterpolatedList (Coords Pos)))
 {- ^ When @a =@ 'AnchorStrings', each 'Evolution' 'ColorString' has exactly one
 corresponding element in the 'SequentiallyInterpolatedList'.

 When @a =@ 'AnchorChars', /each/ 'Evolution' 'ColorString' has exactly @m@
 corresponding elements in the 'SequentiallyInterpolatedList', where @m@ is the
 maximum number of characters in a 'ColorString' of the given 'Evolution' 'ColorString'. -}
 , _textAnimationClock :: {-# UNPACK #-} !EaseClock
 -- ^ Schedules the animation.
} deriving(Show, PrettyVal, Generic)
-- I don't need a 'HasReferencePosition' for now on 'TextAnimation'.
instance GeoTransform (TextAnimation a b) where
  transform f (TextAnimation c d e) = TextAnimation c (transform f d) e

mkEmptyTextAnimation :: TextAnimation a AnchorChars
mkEmptyTextAnimation = TextAnimation [] (Evolution (Successive []) 0 zeroDuration id) (mkEaseClock zeroDuration 0 id)
