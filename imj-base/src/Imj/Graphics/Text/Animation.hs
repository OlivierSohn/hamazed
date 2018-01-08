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
import qualified Prelude(length)

import           Control.Monad( zipWithM_ )
import           Control.Monad.IO.Class(MonadIO)
import           Control.Monad.Reader.Class(MonadReader)

import           Data.Text( unpack, length )
import           Data.List(foldl', splitAt, unzip3)

import           Imj.Geo.Discrete
import           Imj.Graphics.Math.Ease
import           Imj.Graphics.Interpolation
import           Imj.Graphics.Render
import           Imj.Graphics.Text.ColorString


-- | One anchor per String
data AnchorStrings
-- | One anchor per Character
data AnchorChars


-- | Interpolates 'ColorString's and anchors.
data TextAnimation a = TextAnimation {
   _textAnimationEvolutions :: ![Evolution ColorString]
 , _textAnimationAnchors :: !(Evolution (SequentiallyInterpolatedList (Coords Pos)))
 {- ^ When @a =@ 'AnchorStrings', each 'Evolution' 'ColorString' has exactly one
 corresponding element in the 'SequentiallyInterpolatedList'.

 When @a =@ 'AnchorChars', /each/ 'Evolution' 'ColorString' has exactly @m@
 corresponding elements in the 'SequentiallyInterpolatedList', where @m@ is the
 maximum number of characters in a 'ColorString' of the given 'Evolution' 'ColorString'. -}
 , _textAnimationClock :: !EaseClock
 -- ^ Schedules the animation.
} deriving(Show)


-- | Draw a string-anchored 'TextAnimation' for a given 'Frame'
{-# INLINABLE drawAnimatedTextStringAnchored #-}
drawAnimatedTextStringAnchored :: (Draw e, MonadReader e m, MonadIO m)
                               => TextAnimation AnchorStrings
                               -> Frame
                               -> m ()
drawAnimatedTextStringAnchored (TextAnimation fromToStrs anchorsEvolution _) i = do
  let rss = getAnimatedTextAnchors anchorsEvolution i
  drawAnimatedTextStringAnchored' fromToStrs rss i


{-# INLINABLE drawAnimatedTextStringAnchored' #-}
drawAnimatedTextStringAnchored' :: (Draw e, MonadReader e m, MonadIO m)
                                => [Evolution ColorString]
                                -> [Coords Pos]
                                -> Frame
                                -> m ()
drawAnimatedTextStringAnchored' [] _ _ = return ()
drawAnimatedTextStringAnchored' l@(_:_) rs i = do
  let e = head l
      rsNow = head rs
      colorStr = getValueAt e i
  drawColorStr colorStr rsNow
  drawAnimatedTextStringAnchored' (tail l) (tail rs) i

-- | Draw a char-anchored 'TextAnimation' for a given 'Frame'
{-# INLINABLE drawAnimatedTextCharAnchored #-}
drawAnimatedTextCharAnchored :: (Draw e, MonadReader e m, MonadIO m)
                             => TextAnimation AnchorChars
                             -> Frame
                             -> m ()
drawAnimatedTextCharAnchored (TextAnimation fromToStrs anchorsEvolution _) i = do
  let rss = getAnimatedTextAnchors anchorsEvolution i
  drawAnimatedTextCharAnchored' fromToStrs rss i


{-# INLINABLE drawAnimatedTextCharAnchored' #-}
drawAnimatedTextCharAnchored' :: (Draw e, MonadReader e m, MonadIO m)
                              => [Evolution ColorString]
                              -> [Coords Pos]
                              -> Frame
                              -> m ()
drawAnimatedTextCharAnchored' [] _ _ = return ()
drawAnimatedTextCharAnchored' l@(_:_) rs i = do
  -- use length of from to know how many Anchors we should take
  let e@(Evolution (Successive colorStrings) _ _ _) = head l
      nRS = maximum $ map countChars colorStrings
      (nowRS, laterRS) = splitAt nRS rs
      (ColorString colorStr) = getValueAt e i
  drawColorStringAt colorStr nowRS
  drawAnimatedTextCharAnchored' (tail l) laterRS i


{-# INLINABLE drawColorStringAt #-}
drawColorStringAt :: (Draw e, MonadReader e m, MonadIO m)
                  => [(Text, LayeredColor)]
                  -> [Coords Pos]
                  -> m ()
drawColorStringAt [] _ = return ()
drawColorStringAt l@(_:_) rs = do
  let (txt, color) = head l
      len = length txt
      (headRs, tailRs) = splitAt len $ assert (Prelude.length rs >= len) rs
  zipWithM_ (\char coord -> drawChar char coord color) (unpack txt) headRs
  drawColorStringAt (tail l) tailRs

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
mkSequentialTextTranslationsCharAnchored :: [(Successive ColorString, Coords Pos, Coords Pos)]
                                         -- ^ List of (texts, from anchor, to anchor)
                                         -> Float
                                         -- ^ duration in seconds
                                         -> TextAnimation AnchorChars
mkSequentialTextTranslationsCharAnchored l =
  let (from_,to_) =
        foldl'
          (\(froms, tos) (Successive colorStrs, from, to) ->
            let sz = maximum $ map countChars colorStrs
            in (froms ++ build from sz, tos ++ build to sz))
          ([], [])
          l
      txts = map (\(t,_,_) -> t) l
  in mkSequentialTextTranslationsAnchored txts from_ to_

{- | Translates text in an animated way, 'ColorString' by 'ColorString'.

Examples are given in "Imj.Example.SequentialTextTranslationsAnchored".
 -}
mkSequentialTextTranslationsStringAnchored :: [(Successive ColorString, Coords Pos, Coords Pos)]
                                           -- ^ List of (texts, from anchor, to anchor)
                                           -> Float
                                           -- ^ Duration in seconds
                                           -> TextAnimation AnchorStrings
mkSequentialTextTranslationsStringAnchored l =
  let (txts, from_,to_) = unzip3 l
  in mkSequentialTextTranslationsAnchored txts from_ to_

{- | When producing a 'TextAnimation' 'AnchorStrings', /from/ and /to/ anchors
are expected to contain one element per 'Successive' 'ColorString'.

When producing a 'TextAnimation' 'AnchoChars' , /from/ and /to/ anchors
are expected to contain one element per 'Char' in the biggest 'ColorString'
of 'Successive' 'ColorString'. -}
mkSequentialTextTranslationsAnchored :: [Successive ColorString]
                                     -- ^ List of texts
                                     -> [Coords Pos]
                                     -- ^ /From/ anchors
                                     -> [Coords Pos]
                                     -- ^ /To/ anchors
                                     -> Float
                                     -- ^ Duration in seconds
                                     -> TextAnimation a
mkSequentialTextTranslationsAnchored txts from_ to_ duration =
  let strsEv = map (`mkEvolutionEaseQuart` duration) txts
      fromTosLastFrame = maximum $ map (\(Evolution _ lastFrame _ _) -> lastFrame) strsEv
      evAnchors@(Evolution _ anchorsLastFrame _ _) =
        mkEvolutionEaseQuart (Successive [SequentiallyInterpolatedList from_,
                                          SequentiallyInterpolatedList to_]) duration
  in TextAnimation strsEv evAnchors
     $ mkEaseClock duration (max anchorsLastFrame fromTosLastFrame) invQuartEaseInOut


-- | Translates a 'ColorString' between two anchors.
mkTextTranslation :: ColorString
                  -> Float
                  -- ^ Duration in seconds
                  -> Coords Pos
                  -- ^ Left anchor at the beginning
                  -> Coords Pos
                  -- ^ Left anchor at the end
                  -> TextAnimation AnchorChars
mkTextTranslation text duration from to =
  let sz = countChars text
      strEv@(Evolution _ fromToLF _ _) = mkEvolutionEaseQuart (Successive [text]) duration
      from_ = build from sz
      to_ = build to sz
      strsEv = [strEv]
      fromTosLF = fromToLF
      evAnchors@(Evolution _ anchorsLF _ _) =
        mkEvolutionEaseQuart (Successive [SequentiallyInterpolatedList from_,
                                          SequentiallyInterpolatedList to_]) duration
  in TextAnimation strsEv evAnchors $ mkEaseClock duration (max anchorsLF fromTosLF) invQuartEaseInOut
