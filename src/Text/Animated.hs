{-# LANGUAGE NoImplicitPrelude #-}

module Text.Animated
         ( TextAnimation(..)
         , renderAnimatedText
         , renderAnimatedText'
         , getAnimatedTextRenderStates
         , mkTextTranslation
         , mkSequentialTextTranslations
         -- | reexports
         , module Evolution
         ) where

import           Imajuscule.Prelude
import qualified Prelude(length)

import           Control.Monad( zipWithM_ )
import           Data.Text( unpack, length )
import           Data.List(foldl', splitAt)

import           Evolution

import           Render.Console
import           Render

import           Text.ColorString


-- | To animate (in parallel) :
--    - the locations of each individual character
--    - the chars (replacements, inserts, deletes)
--    - the colors
data TextAnimation = TextAnimation {
   _fromTos :: ![Evolution ColorString]
 , _anchorsFrom :: !(Evolution (SequentiallyInterpolatedList RenderState))
}

renderAnimatedText :: TextAnimation -> Frame -> IO ()
renderAnimatedText (TextAnimation fromToStrs fromRenderStatesEvolution) i = do
  let rss = getAnimatedTextRenderStates fromRenderStatesEvolution i
  renderAnimatedText' fromToStrs rss i

renderAnimatedText' :: [Evolution ColorString] -> [RenderState] -> Frame -> IO ()
renderAnimatedText' [] _ _ = return ()
renderAnimatedText' l@(_:_) rs i = do
  -- use length of from to know how many renderstates we should take
  let e@(Evolution from to _ _ _) = head l
      nRS = max (countChars from) (countChars to)
      (nowRS, laterRS) = splitAt nRS rs
      (ColorString colorStr) = fst $ evolve e i
  renderColorStringAt colorStr nowRS
  renderAnimatedText' (tail l) laterRS i

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
  let (SequentiallyInterpolatedList l) = fst $ evolve evolution i
  in l

build :: RenderState -> Int -> [RenderState]
build x sz = map (\i -> move i RIGHT x)  [0..pred sz]

-- | order of aimation is: move, change characters, change color
mkSequentialTextTranslations :: [(ColorString, ColorString, RenderState, RenderState)]
                             -- ^ list of text + start anchor + end anchor
                             -> Float
                             -- ^ duration in seconds
                             -> TextAnimation
mkSequentialTextTranslations l duration =
  let (from_,to_) =
        foldl'
          (\(froms, tos) (colorStrFrom, colorStrTo, from, to) ->
            let sz = max (countChars colorStrFrom) (countChars colorStrTo)
            in (froms ++ build from sz, tos ++ build to sz)
          ) ([], [])
          l
      fromTos = map (\(f,t,_,_) -> mkEvolution f t duration {-- note that using duration here
      makes little sense as we will ignore the duration, the timing will be given by the other evolution.
      TODO maybe use sequential animations-}) l
  in TextAnimation fromTos $
      mkEvolution (SequentiallyInterpolatedList from_)
                  (SequentiallyInterpolatedList to_) duration


-- | In this animation, the beginning and end states are text written horizontally
mkTextTranslation :: ColorString
                  -> Float
                  -- ^ duration in seconds
                  -> RenderState
                  -- ^ left anchor at the beginning
                  -> RenderState
                  -- ^ left anchor at the end
                  -> TextAnimation
mkTextTranslation text duration from to =
  let sz = countChars text
  in TextAnimation [mkEvolution text text duration] $
       mkEvolution (SequentiallyInterpolatedList $ build from sz)
                   (SequentiallyInterpolatedList $ build to sz) duration
