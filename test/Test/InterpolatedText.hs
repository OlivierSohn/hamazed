{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.InterpolatedText(testText) where

import Data.Text(unpack, length)
import Data.List(mapAccumL)

import Imajuscule.Prelude

import Control.Monad( zipWithM_ )

import Game.World.Space
import Game.World.Size
import Interpolation
import Math
import Render.Console
import Render
import Text.Animated

{--
translateIText :: IText -> Coords -> IText
translateIText (IText str anchors) (Coords r c) = IText str $ map (translate r c) anchors

mkIText :: [(Char, RenderState)] -> IText
mkIText l =
  let str = map fst l
      anchors = map snd l
  in IText str anchors

instance DiscretelyInterpolable IText where

  distance (IText t r) (IText t' r') =
    succ $ sum $ zipWith (\x y -> pred $ distance x y) r r'

  interpolate (IText t r) (IText t' r') progress =
    mkIText $ snd $
      mapAccumL
        (\acc ((et,er),(et',er')) ->
          let d = pred $ distance er er'
              r = interpolate er er' $ clamp acc 0 d
          in (acc-d, (assert (et == et') et, r)))
        progress
        $ zip (zip t r) (zip t' r')
--}

testText :: IO ()
testText = do
  let ta@(TextAnimation str from_ to_) = fromto "he"
      d = distance from_ to_
  beginFrame
  mapM_
    (\i -> do
      let t = getAnimatedTextRenderStates ta i
          v = map (translate (Row (2*i)) (Col 0)) t
      renderAnimatedText' str v
    ) [0..pred d]
  endFrame

fromto :: Text -> TextAnimation
fromto str =
  let r = RenderState $ Coords (Row 10) (Col 12)
      r' = RenderState $ Coords (Row 10) (Col 10)
  in mkTextTranslation str r r'
