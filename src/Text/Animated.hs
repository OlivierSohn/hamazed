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

import           Math

import           Render.Console
import           Render

import           Text.ColorString


-- | To animate (in parallel) :
--    - the locations of either :
--      - each ColorString (use a = 'AnchorStrings')
--      - or each character (use a = 'AnchorChars')
--    - chars replacements, inserts, deletes
--    - chars color changes
data (Show a) => TextAnimation a = TextAnimation {
   _textAnimationFromTos :: ![Evolution ColorString] -- TODO is it equivalent to Evolution [ColorString]?
 , _textAnimationAnchorsFrom :: !(Evolution (SequentiallyInterpolatedList RenderState))
 , _textAnimationClock :: !EaseClock
} deriving(Show)

data AnchorStrings = AnchorStrings deriving(Show)
data AnchorChars = AnchorChars deriving(Show)

renderAnimatedTextStringAnchored :: TextAnimation AnchorStrings -> Frame -> IO ()
renderAnimatedTextStringAnchored (TextAnimation fromToStrs renderStatesEvolution _) i = do
  let rss = getAnimatedTextRenderStates renderStatesEvolution i
  renderAnimatedTextStringAnchored' fromToStrs rss i

renderAnimatedTextStringAnchored' :: [Evolution ColorString] -> [RenderState] -> Frame -> IO ()
renderAnimatedTextStringAnchored' [] _ _ = return ()
renderAnimatedTextStringAnchored' l@(_:_) rs i = do
  let e = head l
      rsNow = head rs
      colorStr = evolve e i
  renderColored colorStr rsNow
  renderAnimatedTextStringAnchored' (tail l) (tail rs) i

renderAnimatedTextCharAnchored :: TextAnimation AnchorChars -> Frame -> IO ()
renderAnimatedTextCharAnchored (TextAnimation fromToStrs renderStatesEvolution _) i = do
  let rss = getAnimatedTextRenderStates renderStatesEvolution i
  renderAnimatedTextCharAnchored' fromToStrs rss i

renderAnimatedTextCharAnchored' :: [Evolution ColorString] -> [RenderState] -> Frame -> IO ()
renderAnimatedTextCharAnchored' [] _ _ = return ()
renderAnimatedTextCharAnchored' l@(_:_) rs i = do
  -- use length of from to know how many renderstates we should take
  let e@(Evolution (Successive colorStrings) _ _ _) = head l
      nRS = maximum $ map countChars colorStrings
      (nowRS, laterRS) = splitAt nRS rs
      (ColorString colorStr) = evolve e i
  renderColorStringAt colorStr nowRS
  renderAnimatedTextCharAnchored' (tail l) laterRS i

-- TODO change design of ColorString (replace Colors in ColorString by RenderState) to avoid superfluous info here (Colors of RenderState are ignored)
renderColorStringAt :: [(Text, Color8Code)] -> [RenderState] -> IO ()
renderColorStringAt [] _ = return ()
renderColorStringAt l@(_:_) rs = do
  let (txt, color) = head l
      len = length txt
      (headRs, tailRs) = splitAt len $ assert (Prelude.length rs >= len) rs
  zipWithM_ renderChar_ (unpack txt) $ map (setColor Foreground color) headRs
  renderColorStringAt (tail l) tailRs

getAnimatedTextRenderStates :: Evolution (SequentiallyInterpolatedList RenderState) -> Frame -> [RenderState]
getAnimatedTextRenderStates evolution i =
  let (SequentiallyInterpolatedList l) = evolve evolution i
  in l

build :: RenderState -> Int -> [RenderState]
build x sz = map (\i -> move i RIGHT x)  [0..pred sz]

-- | order of animation is: move, change characters, change color
mkSequentialTextTranslationsCharAnchored :: [([ColorString], RenderState, RenderState)]
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

mkSequentialTextTranslationsStringAnchored :: [([ColorString], RenderState, RenderState)]
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
                  -> RenderState
                  -- ^ left anchor at the beginning
                  -> RenderState
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
