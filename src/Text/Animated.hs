{-# LANGUAGE NoImplicitPrelude #-}

module Text.Animated
         ( TextAnimation(..)
         , renderAnimatedText
         , renderAnimatedText'
         , getAnimatedTextRenderStates
         , mkTextTranslation
         , mkSequentialTextTranslations
         ) where

import           Imajuscule.Prelude
import qualified Prelude(length)

import           Control.Monad( zipWithM_ )
import           Data.Text( unpack, length )
import           Data.List(foldl', splitAt)

import           Interpolation

import           Render.Console
import           Render

import           Text.ColorString


data TextAnimation = TextAnimation {
   _text :: !ColorString
 , _anchors :: !(Evolution (SequentiallyInterpolatedList RenderState))
}

renderAnimatedText :: TextAnimation -> Frame -> IO ()
renderAnimatedText ta@(TextAnimation (ColorString str) _) i = do
  let rss = getAnimatedTextRenderStates ta i
  renderAnimatedText' str rss

renderAnimatedText' :: [(Text, Color8Code)] -> [RenderState] -> IO ()
renderAnimatedText' [] _ = return ()
renderAnimatedText' l@(_:_) rs = do
  let (txt, color) = head l
      len = length txt
      (headRs, tailRs) = splitAt len $ assert (Prelude.length rs >= len) rs
  fg <- setRawForeground color
  zipWithM_ renderChar_ (unpack txt) headRs
  restoreForeground fg
  renderAnimatedText' (tail l) tailRs

getAnimatedTextRenderStates :: TextAnimation -> Frame -> [RenderState]
getAnimatedTextRenderStates (TextAnimation _ evolution) i =
  let (SequentiallyInterpolatedList l) = fst $ evolve evolution i
  in l

build :: RenderState -> Int -> [RenderState]
build x sz = map (\i -> move i RIGHT x)  [0..pred sz]

mkSequentialTextTranslations :: [(ColorString, RenderState, RenderState)]
                             -- ^ list of text + start anchor + end anchor
                             -> Float
                             -- ^ duration in seconds
                             -> TextAnimation
mkSequentialTextTranslations l duration =
  let (txt_,from_,to_) =
        foldl'
          (\(colorstrs, froms, tos) (colorstr, from, to) ->
            let sz = countChars colorstr
            in (colorstrs <> colorstr, froms ++ build from sz, tos ++ build to sz)
          ) (mempty, [], [])
          l
  in TextAnimation txt_ $
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
  in TextAnimation text $
       mkEvolution (SequentiallyInterpolatedList $ build from sz)
                   (SequentiallyInterpolatedList $ build to sz) duration
