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
         , renderAnimatedTextCharAnchored
         , renderAnimatedTextStringAnchored
         , getAnimatedTextRenderStates
         -- * Reexports
         , module Imj.Graphics.Interpolation
         ) where

import           Imj.Prelude
import qualified Prelude(length)

import           Control.Monad( zipWithM_ )
import           Control.Monad.IO.Class(MonadIO)
import           Control.Monad.Reader.Class(MonadReader)

import           Data.Text( unpack, length )
import           Data.List(foldl', splitAt, unzip)

import           Imj.Geo.Discrete
import           Imj.Graphics.Math.Ease
import           Imj.Graphics.Interpolation
import           Imj.Graphics.Render
import           Imj.Graphics.Text.ColorString


-- | One anchor per String
data AnchorStrings
-- | One anchor per Character
data AnchorChars


-- TODO find a generic implementation: 2 aspects (location and content) are
-- interpolated at the same time.
-- | Interpolates 'ColorString's and anchors.
data TextAnimation a = TextAnimation {
   _textAnimationFromTos :: ![Evolution ColorString] -- TODO is it equivalent to Evolution [ColorString]?
 , _textAnimationAnchorsFrom :: !(Evolution (SequentiallyInterpolatedList (Coords Pos)))
 , _textAnimationClock :: !EaseClock
} deriving(Show)


-- | Render a string-anchored 'TextAnimation' for a given 'Frame'
{-# INLINABLE renderAnimatedTextStringAnchored #-}
renderAnimatedTextStringAnchored :: (Draw e, MonadReader e m, MonadIO m)
                                 => TextAnimation AnchorStrings
                                 -> Frame
                                 -> m ()
renderAnimatedTextStringAnchored (TextAnimation fromToStrs renderStatesEvolution _) i = do
  let rss = getAnimatedTextRenderStates renderStatesEvolution i
  renderAnimatedTextStringAnchored' fromToStrs rss i


{-# INLINABLE renderAnimatedTextStringAnchored' #-}
renderAnimatedTextStringAnchored' :: (Draw e, MonadReader e m, MonadIO m)
                                  => [Evolution ColorString]
                                  -> [Coords Pos]
                                  -> Frame
                                  -> m ()
renderAnimatedTextStringAnchored' [] _ _ = return ()
renderAnimatedTextStringAnchored' l@(_:_) rs i = do
  let e = head l
      rsNow = head rs
      colorStr = getValueAt e i
  drawColorStr colorStr rsNow
  renderAnimatedTextStringAnchored' (tail l) (tail rs) i

-- | Render a char-anchored 'TextAnimation' for a given 'Frame'
{-# INLINABLE renderAnimatedTextCharAnchored #-}
renderAnimatedTextCharAnchored :: (Draw e, MonadReader e m, MonadIO m)
                               => TextAnimation AnchorChars
                               -> Frame
                               -> m ()
renderAnimatedTextCharAnchored (TextAnimation fromToStrs renderStatesEvolution _) i = do
  let rss = getAnimatedTextRenderStates renderStatesEvolution i
  renderAnimatedTextCharAnchored' fromToStrs rss i


{-# INLINABLE renderAnimatedTextCharAnchored' #-}
renderAnimatedTextCharAnchored' :: (Draw e, MonadReader e m, MonadIO m)
                                => [Evolution ColorString]
                                -> [Coords Pos]
                                -> Frame
                                -> m ()
renderAnimatedTextCharAnchored' [] _ _ = return ()
renderAnimatedTextCharAnchored' l@(_:_) rs i = do
  -- use length of from to know how many renderstates we should take
  let e@(Evolution (Successive colorStrings) _ _ _) = head l
      nRS = maximum $ map countChars colorStrings
      (nowRS, laterRS) = splitAt nRS rs
      (ColorString colorStr) = getValueAt e i
  renderColorStringAt colorStr nowRS
  renderAnimatedTextCharAnchored' (tail l) laterRS i


{-# INLINABLE renderColorStringAt #-}
renderColorStringAt :: (Draw e, MonadReader e m, MonadIO m)
                    => [(Text, LayeredColor)]
                    -> [Coords Pos]
                    -> m ()
renderColorStringAt [] _ = return ()
renderColorStringAt l@(_:_) rs = do
  let (txt, color) = head l
      len = length txt
      (headRs, tailRs) = splitAt len $ assert (Prelude.length rs >= len) rs
  zipWithM_ (\char coord -> drawChar char coord color) (unpack txt) headRs
  renderColorStringAt (tail l) tailRs

getAnimatedTextRenderStates :: Evolution (SequentiallyInterpolatedList (Coords Pos))
                            -> Frame
                            -> [Coords Pos]
getAnimatedTextRenderStates evolution i =
  let (SequentiallyInterpolatedList l) = getValueAt evolution i
  in l

build :: Coords Pos -> Int -> [Coords Pos]
build x sz = map (\i -> move i RIGHT x)  [0..pred sz]

{- | Translates text in an animated way,ete character by character.

Examples are given in "Imj.Example.SequentialTextTranslationsAnchored".
 -}
mkSequentialTextTranslationsCharAnchored :: [([ColorString], Coords Pos, Coords Pos)]
                                         -- ^ List of (texts, from anchor, to anchor)
                                         -> Float
                                         -- ^ duration in seconds
                                         -> TextAnimation AnchorChars
mkSequentialTextTranslationsCharAnchored l duration =
  let (from_,to_) =
        foldl'
          (\(froms, tos) (colorStrs, from, to) ->
            let sz = maximum $ map countChars colorStrs
            in (froms ++ build from sz, tos ++ build to sz))
          ([], [])
          l
      strsEv = map (\(txts,_,_) -> mkEvolutionEaseQuart (Successive txts) duration) l
      fromTosLF = maximum $ map (\(Evolution _ lf _ _) -> lf) strsEv
      evAnchors@(Evolution _ anchorsLF _ _) =
        mkEvolutionEaseQuart (Successive [SequentiallyInterpolatedList from_,
                                          SequentiallyInterpolatedList to_]) duration
  in TextAnimation strsEv evAnchors $ mkEaseClock duration (max anchorsLF fromTosLF) invQuartEaseInOut

{- | Translates text in an animated way, 'ColorString' by 'ColorString'.

Examples are given in "Imj.Example.SequentialTextTranslationsAnchored".
 -}
mkSequentialTextTranslationsStringAnchored :: [([ColorString], Coords Pos, Coords Pos)]
                                           -- ^ List of (texts, from anchor, to anchor)
                                           -> Float
                                           -- ^ Duration in seconds
                                           -> TextAnimation AnchorStrings
mkSequentialTextTranslationsStringAnchored l duration =
  let (from_,to_) = unzip $ map (\(_,f,t) -> (f,t)) l
      strsEv = map (\(txts,_,_) -> mkEvolutionEaseQuart (Successive txts) duration) l
      fromTosLF = maximum $ map (\(Evolution _ lf _ _) -> lf) strsEv
      evAnchors@(Evolution _ anchorsLF _ _) =
        mkEvolutionEaseQuart (Successive [SequentiallyInterpolatedList from_,
                                          SequentiallyInterpolatedList to_]) duration
  in TextAnimation strsEv evAnchors $ mkEaseClock duration (max anchorsLF fromTosLF) invQuartEaseInOut


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
