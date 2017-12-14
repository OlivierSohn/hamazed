{-# LANGUAGE NoImplicitPrelude #-}

module Text.Animated
         ( TextAnimation(..)
         , AnchorChars
         , AnchorStrings
         , renderAnimatedTextCharAnchored
         , renderAnimatedTextStringAnchored
         , getAnimatedTextRenderStates
         , mkTextTranslation
         , mkSequentialTextTranslationsCharAnchored
         , mkSequentialTextTranslationsStringAnchored
         -- | reexports
         , module Evolution
         ) where

import           Imajuscule.Prelude
import qualified Prelude(length)

import           Control.Monad( zipWithM_ )
import           Data.Text( unpack, length )
import           Data.List(foldl', splitAt, unzip)

import           Evolution

import           Geo.Discrete

import           Math

import           Text.ColorString

{- | Animates in parallel

* The locations of either

    * each ColorString (use a = 'AnchorStrings')
    * or each character (use a = 'AnchorChars')
* characters replacements, inserts, deletes
* characters color changes
-}
data (Show a) => TextAnimation a = TextAnimation {
   _textAnimationFromTos :: ![Evolution ColorString] -- TODO is it equivalent to Evolution [ColorString]?
 , _textAnimationAnchorsFrom :: !(Evolution (SequentiallyInterpolatedList Coords))
 , _textAnimationClock :: !EaseClock
} deriving(Show)

data AnchorStrings = AnchorStrings deriving(Show)
data AnchorChars = AnchorChars deriving(Show)

renderAnimatedTextStringAnchored :: TextAnimation AnchorStrings
                                 -> Frame
                                 -> (Text -> Coords -> LayeredColor -> IO ())
                                 -> IO ()
renderAnimatedTextStringAnchored (TextAnimation fromToStrs renderStatesEvolution _) i r = do
  let rss = getAnimatedTextRenderStates renderStatesEvolution i
  renderAnimatedTextStringAnchored' fromToStrs rss i r

renderAnimatedTextStringAnchored' :: [Evolution ColorString]
                                  -> [Coords]
                                  -> Frame
                                  -> (Text -> Coords -> LayeredColor -> IO ())
                                  -> IO ()
renderAnimatedTextStringAnchored' [] _ _ _ = return ()
renderAnimatedTextStringAnchored' l@(_:_) rs i r = do
  let e = head l
      rsNow = head rs
      colorStr = evolve e i
  renderColored colorStr rsNow r
  renderAnimatedTextStringAnchored' (tail l) (tail rs) i r

renderAnimatedTextCharAnchored :: TextAnimation AnchorChars
                               -> Frame
                               -> (Char -> Coords -> LayeredColor -> IO ())
                               -> IO ()
renderAnimatedTextCharAnchored (TextAnimation fromToStrs renderStatesEvolution _) i renderChar = do
  let rss = getAnimatedTextRenderStates renderStatesEvolution i
  renderAnimatedTextCharAnchored' fromToStrs rss i renderChar

renderAnimatedTextCharAnchored' :: [Evolution ColorString]
                                -> [Coords]
                                -> Frame
                                -> (Char -> Coords -> LayeredColor -> IO ())
                                -> IO ()
renderAnimatedTextCharAnchored' [] _ _ _ = return ()
renderAnimatedTextCharAnchored' l@(_:_) rs i renderChar = do
  -- use length of from to know how many renderstates we should take
  let e@(Evolution (Successive colorStrings) _ _ _) = head l
      nRS = maximum $ map countChars colorStrings
      (nowRS, laterRS) = splitAt nRS rs
      (ColorString colorStr) = evolve e i
  renderColorStringAt colorStr nowRS renderChar
  renderAnimatedTextCharAnchored' (tail l) laterRS i renderChar

renderColorStringAt :: [(Text, LayeredColor)]
                    -> [Coords]
                    -> (Char -> Coords -> LayeredColor -> IO ())
                    -> IO ()
renderColorStringAt [] _ _ = return ()
renderColorStringAt l@(_:_) rs renderChar = do
  let (txt, color) = head l
      len = length txt
      (headRs, tailRs) = splitAt len $ assert (Prelude.length rs >= len) rs
  zipWithM_ (\char coord -> renderChar char coord color) (unpack txt) headRs
  renderColorStringAt (tail l) tailRs renderChar

getAnimatedTextRenderStates :: Evolution (SequentiallyInterpolatedList Coords)
                            -> Frame
                            -> [Coords]
getAnimatedTextRenderStates evolution i =
  let (SequentiallyInterpolatedList l) = evolve evolution i
  in l

build :: Coords -> Int -> [Coords]
build x sz = map (\i -> move i RIGHT x)  [0..pred sz]

-- | order of animation is: move, change characters, change color
mkSequentialTextTranslationsCharAnchored :: [([ColorString], Coords, Coords)]
                                         -- ^ list of text + start anchor + end anchor
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
      strsEv = map (\(txts,_,_) -> mkEvolution (Successive txts) duration) l
      fromTosLF = maximum $ map (\(Evolution _ lf _ _) -> lf) strsEv
      evAnchors@(Evolution _ anchorsLF _ _) =
        mkEvolution2 (SequentiallyInterpolatedList from_)
                     (SequentiallyInterpolatedList to_) duration
  in TextAnimation strsEv evAnchors $ mkEaseClock duration (max anchorsLF fromTosLF) invQuartEaseInOut

mkSequentialTextTranslationsStringAnchored :: [([ColorString], Coords, Coords)]
                                           -- ^ list of texts, start anchor, end anchor
                                           -> Float
                                           -- ^ duration in seconds
                                           -> TextAnimation AnchorStrings
mkSequentialTextTranslationsStringAnchored l duration =
  let (from_,to_) = unzip $ map (\(_,f,t) -> (f,t)) l
      strsEv = map (\(txts,_,_) -> mkEvolution (Successive txts) duration) l
      fromTosLF = maximum $ map (\(Evolution _ lf _ _) -> lf) strsEv
      evAnchors@(Evolution _ anchorsLF _ _) =
        mkEvolution2 (SequentiallyInterpolatedList from_)
                     (SequentiallyInterpolatedList to_) duration
  in TextAnimation strsEv evAnchors $ mkEaseClock duration (max anchorsLF fromTosLF) invQuartEaseInOut


-- | In this animation, the beginning and end states are text written horizontally
mkTextTranslation :: ColorString
                  -> Float
                  -- ^ duration in seconds
                  -> Coords
                  -- ^ left anchor at the beginning
                  -> Coords
                  -- ^ left anchor at the end
                  -> TextAnimation AnchorChars
mkTextTranslation text duration from to =
  let sz = countChars text
      strEv@(Evolution _ fromToLF _ _) = mkEvolution1 text duration
      from_ = build from sz
      to_ = build to sz
      strsEv = [strEv]
      fromTosLF = fromToLF
      evAnchors@(Evolution _ anchorsLF _ _) =
        mkEvolution2 (SequentiallyInterpolatedList from_)
                     (SequentiallyInterpolatedList to_) duration
  in TextAnimation strsEv evAnchors $ mkEaseClock duration (max anchorsLF fromTosLF) invQuartEaseInOut
