{-# LANGUAGE NoImplicitPrelude #-}

module Text.Animated
         ( TextAnimation(..)
         , renderAnimatedText
         , getAnimatedTextRenderStates
         ) where

import           Imajuscule.Prelude

import           Control.Monad( zipWithM_ )
import           Data.Text( unpack )
import           Interpolation

import           Render.Console


data TextAnimation = TextAnimation {
   _text :: !Text
 , _anchorsFrom :: !(SequentiallyInterpolatedList RenderState)
 , _anchorsTo :: !(SequentiallyInterpolatedList RenderState)
} deriving(Eq, Ord)

renderAnimatedText :: Text -> [RenderState] -> IO ()
renderAnimatedText =
  zipWithM_ renderChar_ . unpack

getAnimatedTextRenderStates :: TextAnimation -> Int -> [RenderState]
getAnimatedTextRenderStates (TextAnimation _ from_ to_) i =
  let (SequentiallyInterpolatedList l) = interpolate from_ to_ i
  in l
