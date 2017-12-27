{-# OPTIONS_HADDOCK prune #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Text.Animation
         (
         -- * TextAnimation
{- |
Animates in parallel:

* characters replacements, inserts, deletes
* characters color changes
* if a = 'AnchorStrings' : the locations of ColorStings
* if a = 'AnchorChars' : the locations of each individual character
-}
           TextAnimation(..)
         , AnchorChars
         , AnchorStrings
         -- * Constructors
         , mkTextTranslation
         , mkSequentialTextTranslationsCharAnchored
         , mkSequentialTextTranslationsStringAnchored
         -- * Drawing
         , renderAnimatedTextCharAnchored
         , renderAnimatedTextStringAnchored
         , getAnimatedTextRenderStates
         -- * Reexports
         , module Imj.Interpolation
         ) where

import           Imj.Prelude
import qualified Prelude(length)

import           Control.Monad( zipWithM_ )
import           Control.Monad.IO.Class(MonadIO)
import           Control.Monad.Reader.Class(MonadReader)

import           Data.Text( unpack, length )
import           Data.List(foldl', splitAt, unzip)

import           Imj.Draw
import           Imj.Geo.Discrete
import           Imj.Math.Ease
import           Imj.Interpolation
import           Imj.Text.ColorString


-- | One anchor per String
data AnchorStrings
-- | One anchor per Character
data AnchorChars


-- TODO find a generic implementation: 2 aspects (location and content) are
-- interpolated at the same time.
-- | Animates a 'ColorString' content and anchors.
data TextAnimation a = TextAnimation {
   _textAnimationFromTos :: ![Evolution ColorString] -- TODO is it equivalent to Evolution [ColorString]?
 , _textAnimationAnchorsFrom :: !(Evolution (SequentiallyInterpolatedList Coords))
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
                                  -> [Coords]
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
                                -> [Coords]
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
                    -> [Coords]
                    -> m ()
renderColorStringAt [] _ = return ()
renderColorStringAt l@(_:_) rs = do
  let (txt, color) = head l
      len = length txt
      (headRs, tailRs) = splitAt len $ assert (Prelude.length rs >= len) rs
  zipWithM_ (\char coord -> drawChar char coord color) (unpack txt) headRs
  renderColorStringAt (tail l) tailRs

getAnimatedTextRenderStates :: Evolution (SequentiallyInterpolatedList Coords)
                            -> Frame
                            -> [Coords]
getAnimatedTextRenderStates evolution i =
  let (SequentiallyInterpolatedList l) = getValueAt evolution i
  in l

build :: Coords -> Int -> [Coords]
build x sz = map (\i -> move i RIGHT x)  [0..pred sz]

-- | The order of animation is: move, change characters, change color
mkSequentialTextTranslationsCharAnchored :: [([ColorString], Coords, Coords)]
                                         -- ^ List of (text + from anchor + to anchor)
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

-- | Same as 'mkSequentialTextTranslationsCharAnchored' except it is String anchored
mkSequentialTextTranslationsStringAnchored :: [([ColorString], Coords, Coords)]
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
                  -> Coords
                  -- ^ Left anchor at the beginning
                  -> Coords
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
