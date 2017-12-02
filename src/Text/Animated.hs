{-# LANGUAGE NoImplicitPrelude #-}

module Text.Animated
         ( TextAnimation(..)
         , renderAnimatedText
         , renderAnimatedText'
         , getAnimatedTextRenderStates
         , mkTextTranslation
         ) where

import           Imajuscule.Prelude

import           Control.Monad( zipWithM_ )
import           Data.Text( unpack, length )
import           Interpolation

import           Render.Console
import           Render


data TextAnimation = TextAnimation {
   _text :: !Text
 , _anchorsFrom :: !(SequentiallyInterpolatedList RenderState)
 , _anchorsTo :: !(SequentiallyInterpolatedList RenderState)
} deriving(Eq, Ord)

renderAnimatedText :: TextAnimation -> Int -> IO ()
renderAnimatedText ta@(TextAnimation str _ _) i = do
  let rss = getAnimatedTextRenderStates ta i
  renderAnimatedText' str rss

renderAnimatedText' :: Text -> [RenderState] -> IO ()
renderAnimatedText' =
  zipWithM_ renderChar_ . unpack

getAnimatedTextRenderStates :: TextAnimation -> Int -> [RenderState]
getAnimatedTextRenderStates (TextAnimation _ from_ to_) i =
  let (SequentiallyInterpolatedList l) = interpolate from_ to_ i
  in l

mkTextTranslation :: Text -> RenderState -> RenderState -> TextAnimation
mkTextTranslation text from to =
  let sz = length text
      build x = SequentiallyInterpolatedList $ map (\i -> move i Down x)  [0..pred sz]
  in TextAnimation text (build from) (build to)
