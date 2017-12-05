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

import           Render.Console
import           Render

import           Text.ColorString


-- | To animate (in parallel) :
--    - the locations of either :
--      - each ColorString (use a = 'AnchorStrings')
--      - or each character (use a = 'AnchorChars')
--    - chars replacements, inserts, deletes
--    - chars color changes
data TextAnimation a = TextAnimation {
   _fromTos :: ![Evolution ColorString]
 , _anchorsFrom :: !(Evolution (SequentiallyInterpolatedList RenderState))
}

data AnchorStrings
data AnchorChars

renderAnimatedTextStringAnchored :: TextAnimation AnchorStrings -> Frame -> IO ()
renderAnimatedTextStringAnchored (TextAnimation fromToStrs renderStatesEvolution) i = do
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
renderAnimatedTextCharAnchored (TextAnimation fromToStrs renderStatesEvolution) i = do
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

renderColorStringAt :: [(Text, Color8Code)] -> [RenderState] -> IO ()
renderColorStringAt [] _ = return ()
renderColorStringAt l@(_:_) rs = do
  let (txt, color) = head l
      len = length txt
      (headRs, tailRs) = splitAt len $ assert (Prelude.length rs >= len) rs
  fg <- setRawForeground color
  zipWithM_ renderChar_ (unpack txt) headRs
  restoreForeground fg
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
  in TextAnimation strsEv $
      mkEvolution2 (SequentiallyInterpolatedList from_)
                   (SequentiallyInterpolatedList to_) duration

-- | order of animation is: move, change characters, change color
mkSequentialTextTranslationsStringAnchored :: [([ColorString], RenderState, RenderState)]
                                           -- ^ list of texts, start anchor, end anchor
                                           -> Float
                                           -- ^ duration in seconds
                                           -> TextAnimation AnchorStrings
mkSequentialTextTranslationsStringAnchored l duration =
  let (from_,to_) = unzip $ map (\(_,f,t) -> (f,t)) l
      strsEv = map (\(txts,_,_) -> mkEvolution (Successive txts) duration) l
  in TextAnimation strsEv $
      mkEvolution2 (SequentiallyInterpolatedList from_)
                   (SequentiallyInterpolatedList to_) duration


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
  in TextAnimation [mkEvolution1 text duration] $
       mkEvolution2 (SequentiallyInterpolatedList $ build from sz)
                    (SequentiallyInterpolatedList $ build to sz) duration
