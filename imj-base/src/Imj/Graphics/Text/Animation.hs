{-# OPTIONS_HADDOCK prune #-}
{-# LANGUAGE NoImplicitPrelude #-}

{- |
= Examples

Examples are available in "Imj.Example.SequentialTextTranslationsAnchored":

* Run @imj-base-examples-exe@ to see these examples displayed in the terminal
-}
module Imj.Graphics.Text.Animation
         (
         -- * TextAnimation
{- |
Interpolates between various 'ColorString's, and /at the same time/ interpolates
their anchors.

Anchors interpolation can occur :

* at the 'ColorString' level using 'AnchorStrings', or
* at the 'Char' level using 'AnchorChars' -}
           TextAnimation(..)
         , AnchorChars
         , AnchorStrings
         -- * Constructors
         , mkTextTranslation
         , mkSequentialTextTranslationsCharAnchored
         , mkSequentialTextTranslationsStringAnchored
         -- * Draw
         , drawAnimatedTextCharAnchored
         , drawAnimatedTextStringAnchored
         , getAnimatedTextAnchors
         -- * Reexports
         , module Imj.Graphics.Interpolation
         ) where

import           Imj.Prelude

import           Data.List(foldl', splitAt, unzip3)

import           Imj.Geo.Discrete.Types
import           Imj.Graphics.Text.Animation.Types

import           Imj.Graphics.Math.Ease
import           Imj.Graphics.Interpolation
import           Imj.Graphics.Class.Positionable
import           Imj.Graphics.Class.Words(Characters)
import qualified Imj.Graphics.Class.Words as Words
import           Imj.Graphics.Render
import           Imj.Timing

-- | Draw a string-anchored 'TextAnimation' for a given 'Frame'
{-# INLINABLE drawAnimatedTextStringAnchored #-}
drawAnimatedTextStringAnchored :: (Positionable a, DiscreteInterpolation a
                                  , Draw e, MonadReader e m, MonadIO m)
                               => TextAnimation a AnchorStrings
                               -> Frame
                               -> m ()
drawAnimatedTextStringAnchored (TextAnimation fromToStrs anchorsEvolution _) i = do
  let rss = getAnimatedTextAnchors anchorsEvolution i
  drawAnimatedTextStringAnchored' fromToStrs rss i


{-# INLINABLE drawAnimatedTextStringAnchored' #-}
drawAnimatedTextStringAnchored' :: (Positionable a, DiscreteInterpolation a
                                   , Draw e, MonadReader e m, MonadIO m)
                                => [Evolution a]
                                -> [Coords Pos]
                                -> Frame
                                -> m ()
drawAnimatedTextStringAnchored' (e:es) (r:rs) i = do
  let colorStr = getValueAt e i
  drawAt colorStr r
  drawAnimatedTextStringAnchored' es rs i
drawAnimatedTextStringAnchored' _ _ _ = return ()

-- | Draw a char-anchored 'TextAnimation' for a given 'Frame'
{-# INLINABLE drawAnimatedTextCharAnchored #-}
drawAnimatedTextCharAnchored :: (DiscreteInterpolation a, Characters a,
                                 Draw e, MonadReader e m, MonadIO m)
                             => TextAnimation a AnchorChars
                             -> Frame
                             -> m ()
drawAnimatedTextCharAnchored (TextAnimation fromToStrs anchorsEvolution _) i = do
  let rss = getAnimatedTextAnchors anchorsEvolution i
  drawAnimatedTextCharAnchored' fromToStrs rss i


{-# INLINABLE drawAnimatedTextCharAnchored' #-}
drawAnimatedTextCharAnchored' :: (DiscreteInterpolation a, Characters a,
                                  Draw e, MonadReader e m, MonadIO m)
                              => [Evolution a]
                              -> [Coords Pos]
                              -> Frame
                              -> m ()
drawAnimatedTextCharAnchored' [] _ _ = return ()
drawAnimatedTextCharAnchored' (e@(Evolution (Successive colorStrings) _ _ _):rest) rs i = do
  -- use length of from to know how many Anchors we should take
  let nRS = fromMaybe (error "logic") $ maximumMaybe $ map Words.length colorStrings
      (nowRS, laterRS) = splitAt nRS rs
      str = getValueAt e i
  Words.drawOnPath nowRS str
  drawAnimatedTextCharAnchored' rest laterRS i

getAnimatedTextAnchors :: Evolution (SequentiallyInterpolatedList (Coords Pos))
                       -> Frame
                       -> [Coords Pos]
getAnimatedTextAnchors evolution i =
  let (SequentiallyInterpolatedList l) = getValueAt evolution i
  in l

build :: Coords Pos -> Int -> [Coords Pos]
build x sz = map (\i -> move i RIGHT x)  [0..pred sz]

{- | Translates text in an animated way,ete character by character.

Examples are given in "Imj.Example.SequentialTextTranslationsAnchored".
 -}
mkSequentialTextTranslationsCharAnchored :: (DiscreteDistance a, Characters a)
                                         => [(Successive a, Coords Pos, Coords Pos)]
                                         -- ^ List of (texts, from anchor, to anchor)
                                         -> Time Duration System
                                         -- ^ duration
                                         -> TextAnimation a AnchorChars
mkSequentialTextTranslationsCharAnchored l =
  let (from_,to_) =
        foldl'
          (\(froms, tos) (Successive a, from, to) ->
            let sz = fromMaybe (error "logic") $ maximumMaybe $ map Words.length a
            in (froms ++ build from sz, tos ++ build to sz))
          ([], [])
          l
      txts = map (\(t,_,_) -> t) l
  in mkSequentialTextTranslationsAnchored txts from_ to_

{- | Translates text in an animated way, 'ColorString' by 'ColorString'.

Examples are given in "Imj.Example.SequentialTextTranslationsAnchored".
 -}
mkSequentialTextTranslationsStringAnchored :: (DiscreteDistance a)
                                           => [(Successive a, Coords Pos, Coords Pos)]
                                           -- ^ List of (texts, from anchor, to anchor)
                                           -> Time Duration System
                                           -- ^ Duration
                                           -> TextAnimation a AnchorStrings
mkSequentialTextTranslationsStringAnchored l =
  let (txts, from_,to_) = unzip3 l
  in mkSequentialTextTranslationsAnchored txts from_ to_

{- | When producing a 'TextAnimation' 'AnchorStrings', /from/ and /to/ anchors
are expected to contain one element per 'Successive' 'ColorString'.

When producing a 'TextAnimation' 'AnchoChars' , /from/ and /to/ anchors
are expected to contain one element per 'Char' in the biggest 'ColorString'
of 'Successive' 'ColorString'. -}
mkSequentialTextTranslationsAnchored :: (DiscreteDistance a)
                                     => [Successive a]
                                     -- ^ List of texts
                                     -> [Coords Pos]
                                     -- ^ /From/ anchors
                                     -> [Coords Pos]
                                     -- ^ /To/ anchors
                                     -> Time Duration System
                                     -- ^ Duration
                                     -> TextAnimation a b
mkSequentialTextTranslationsAnchored txts from_ to_ duration =
  let strsEv = map (`mkEvolutionEaseQuart` duration) txts
      fromTosLastFrame = fromMaybe 0 $ maximumMaybe $ map (\(Evolution _ lastFrame _ _) -> lastFrame) strsEv
      evAnchors@(Evolution _ anchorsLastFrame _ _) =
        mkEvolutionEaseQuart (Successive [SequentiallyInterpolatedList from_,
                                          SequentiallyInterpolatedList to_]) duration
  in TextAnimation strsEv evAnchors
     $ mkEaseClock duration (max anchorsLastFrame fromTosLastFrame) invQuartEaseInOut


-- | Translates a 'ColorString' between two anchors.
mkTextTranslation :: (DiscreteDistance a, Characters a)
                  => a
                  -> Time Duration System
                  -- ^ Duration
                  -> Coords Pos
                  -- ^ Left anchor at the beginning
                  -> Coords Pos
                  -- ^ Left anchor at the end
                  -> TextAnimation a AnchorChars
mkTextTranslation text duration from to =
  let sz = Words.length text
      strEv@(Evolution _ fromToLF _ _) = mkEvolutionEaseQuart (Successive [text]) duration
      from_ = build from sz
      to_ = build to sz
      strsEv = [strEv]
      fromTosLF = fromToLF
      evAnchors@(Evolution _ anchorsLF _ _) =
        mkEvolutionEaseQuart (Successive [SequentiallyInterpolatedList from_,
                                          SequentiallyInterpolatedList to_]) duration
  in TextAnimation strsEv evAnchors $ mkEaseClock duration (max anchorsLF fromTosLF) invQuartEaseInOut
