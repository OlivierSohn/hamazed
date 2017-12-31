{-# LANGUAGE OverloadedStrings #-}

{- | Animated text examples by comparing these functions:

* 'mkSequentialTextTranslationsCharAnchored' / 'renderAnimatedTextCharAnchored'
* 'mkSequentialTextTranslationsStringAnchored' / 'renderAnimatedTextStringAnchored'

The results of the animations are displayed in a grid, then it's easy to compare
char-anchored animations with string-anchored animations.

Each example comes with a comment displayed in the console.

To run these example, execute @imj-base-examples-exe@.
-}

module Imj.Example.SequentialTextTranslationsAnchored
    ( exampleOfsequentialTextTranslationsAnchored
    -- * Reexports
    , module Imj.Graphics.Text.Animation
    ) where

import           Data.Monoid((<>))
import           Data.Text(pack)
import           Control.Concurrent(threadDelay)
import           Control.Monad.IO.Class(MonadIO, liftIO)
import           Control.Monad.Reader.Class(MonadReader)

import           Imj.Geo.Discrete
import           Imj.Graphics.Class.Render
import           Imj.Graphics.Color
import           Imj.Graphics.Render.FromMonadReader
import           Imj.Graphics.Text.Alignment
import           Imj.Graphics.Text.Animation
import           Imj.Graphics.Text.ColorString
import           Imj.Graphics.UI.RectContainer


-- | Shows the differences between 'AnchorChars' and 'AnchorStrings', by comparing,
-- with the same inputs:
--
-- * 'mkSequentialTextTranslationsCharAnchored' / 'renderAnimatedTextCharAnchored'
-- * 'mkSequentialTextTranslationsStringAnchored' / 'renderAnimatedTextStringAnchored'
exampleOfsequentialTextTranslationsAnchored :: (Render e, MonadReader e m, MonadIO m)
                                            => m ()
exampleOfsequentialTextTranslationsAnchored = do
  let (Examples es) = allExamples
      rs = map (\(i, (Example e _ _)) ->
                  let (a,b) = runExampleCharAnchored e ref
                      ref = translate upperLeft $ Coords (fromIntegral height*i) 0
                  in (a,b,ref)) $ zip [0..] es
      rs' = map (\(i, (Example e _ _)) ->
                  let (a,b) = runExampleStringAnchored e ref
                      ref = translate upperLeft $ Coords (fromIntegral height*i) (fromIntegral width)
                  in (a,b,ref)) $ zip [0..] es
  animate (rs ++ rs') es ["Char anchored", "String anchored"]

height :: Length Height
height = 10

width :: Length Width
width = 30

upperLeft :: Coords Pos
upperLeft = Coords 4 50

data Examples = Examples [Example]

data Example = Example {
    _exampleInputData :: ![([ColorString], Coords Pos, Coords Pos)]
  , _exampleName :: !String
  , _exampleComment :: !String
}

runExampleCharAnchored :: (Render e, MonadReader e m, MonadIO m)
                       => [([ColorString], Coords Pos, Coords Pos)]
                       -> Coords Pos
                       -> (Frame, (Frame -> m ()))
runExampleCharAnchored input ref =
  let anim@(TextAnimation _ _ (EaseClock (Evolution _ lastFrame _ _))) =
        mkSequentialTextTranslationsCharAnchored (translateInput ref input) 1
  in (lastFrame, (renderAnimatedTextCharAnchored anim))

runExampleStringAnchored :: (Render e, MonadReader e m, MonadIO m)
                         => [([ColorString], Coords Pos, Coords Pos)]
                         -> Coords Pos
                         -> (Frame, (Frame -> m ()))
runExampleStringAnchored input ref =
  let anim@(TextAnimation _ _ (EaseClock (Evolution _ lastFrame _ _))) =
        mkSequentialTextTranslationsStringAnchored (translateInput ref input) 1
  in (lastFrame, (renderAnimatedTextStringAnchored anim))

translateInput :: Coords Pos
               -> [([ColorString], Coords Pos, Coords Pos)]
               -> [([ColorString], Coords Pos, Coords Pos)]
translateInput tr input =
  map (\(l, c1, c2) -> (l, translate tr c1, translate tr c2)) input

allExamples :: Examples
allExamples =
  Examples
    [ Example exampleColorComplexWithMotionAndTwoStrings
                    "ColorComplexWithMotionAndTwoStrings"
                    "Char and String anchors give different results because there is a move"
    , Example exampleColorComplexWithMotion
                    "ColorComplexWithMotion"
                    "Char and String anchors give different results because there is a move"
    , Example exampleBug
                    "Bug"
                    "Bug: color should change progressively when the char changes"
    , Example exampleColorComplex
                    "ColorComplex"
                    "Char and String anchors give the same result because there is no move"
    , Example exampleColor
                    "Color"
                    "Char and String anchors give the same result because there is no move"
    ]

-- | shows an example with multiple strings : global color is changed in parallel
-- but anchors are changed sequentially
exampleColorComplexWithMotionAndTwoStrings :: [([ColorString], Coords Pos, Coords Pos)]
exampleColorComplexWithMotionAndTwoStrings =
  let a = colored "ABC" green <> colored "DEF" (rgb 2 2 2)
      b = colored "ABC" white <> colored "DEF" yellow
      txt1 = [a, b]
      from1 = Coords 0 0
      to1 = Coords 1 0
      from2 = Coords 0 10
      to2 = Coords 1 10
  in [(txt1, from1, to1)
    , (txt1, from2, to2)]

exampleColorComplexWithMotion :: [([ColorString], Coords Pos, Coords Pos)]
exampleColorComplexWithMotion =
  let a = colored "ABC" green <> colored "DEF" (rgb 2 2 2)
      b = colored "ABC" white <> colored "DEF" yellow
      txt1 = [a, b]
      from1 = Coords 0 0
      to1 = Coords 1 0
  in [(txt1, from1, to1)]

exampleColorComplex :: [([ColorString], Coords Pos, Coords Pos)]
exampleColorComplex =
  let a = colored "ABC" green <> colored "DEF" (rgb 2 2 2)
      b = colored "ABC" white <> colored "DEF" yellow
      txt1 = [a, b]
      from1 = Coords 0 0
      to1 = Coords 0 0
  in [(txt1, from1, to1)]

exampleColor :: [([ColorString], Coords Pos, Coords Pos)]
exampleColor =
  let a = colored "ABC" green
      b = colored "ABC" blue
      txt1 = [a, b]
      from1 = Coords 0 0
      to1 = Coords 0 0
  in [(txt1, from1, to1)]

-- | TODO color should change progressively when the char changes.
exampleBug :: [([ColorString], Coords Pos, Coords Pos)]
exampleBug =
  let a = colored "ABC" green
      b = colored "DEFGH" blue
      txt1 = [a, b]
      from1 = Coords 0 0
      to1 = Coords 4 0
  in [(txt1, from1, to1)]

animate :: (Render e, MonadReader e m, MonadIO m)
        => [(Frame, (Frame -> m ()), Coords Pos)]
        -> [Example]
        -> [String]
        -> m ()
animate listActions examples colTitles = do
  let frames = replicate (length listActions) (Frame 0)
  animate' listActions examples frames colTitles

animate' :: (Render e, MonadReader e m, MonadIO m)
        => [(Frame, (Frame -> m ()), Coords Pos)]
        -> [Example]
        -> [Frame]
        -> [String]
        -> m ()
animate' listActions examples frames colTitles = do
  let newFrames = zipWith (\count (lastFrame, _, _) ->
                              if count >= lastFrame
                                then Frame 0
                                else succ count) frames listActions
  drawActions listActions frames
  drawExamples examples
  drawColTitles colTitles
  renderToScreen
  liftIO $ threadDelay 1000000
  animate' listActions examples newFrames colTitles

myDarkGray :: LayeredColor
myDarkGray = onBlack $ gray 6
myLightGray :: LayeredColor
myLightGray = onBlack $ gray 10

drawActions :: (Render e, MonadReader e m, MonadIO m)
            => [(Frame, (Frame -> m ()), Coords Pos)]
            -> [Frame]
            -> m ()
drawActions listActions frames =
  mapM_ (\(frame, (lastFrame, action, ref)) -> do
            let r = RectContainer (Size (height-2) (width-3)) (translate (Coords (-2) (-2)) ref)
            drawUsingColor r myDarkGray
            action frame
            drawColorStr (progress frame lastFrame) (translate ref $ Coords (fromIntegral height - 4) 0)
            ) $ zip frames listActions

drawExamples :: (Render e, MonadReader e m, MonadIO m)
            => [Example]
            -> m ()
drawExamples examples =
  mapM_ (\(i,(Example _ leftTitle rightComment)) -> do
            let down = (quot (fromIntegral height) 2) + fromIntegral (i*height) - 2
                at = translate upperLeft (Coords down (-8))
                at' = translate upperLeft (Coords down $ 2 * (fromIntegral width))
            drawAlignedTxt_ (pack leftTitle) myDarkGray (mkRightAlign at)
            drawStr rightComment at' myDarkGray
            ) $ zip [0..] examples

drawColTitles :: (Render e, MonadReader e m, MonadIO m)
            => [String]
            -> m ()
drawColTitles l =
    mapM_ (\(i,colTitle) -> do
              let right = quot (fromIntegral width) 2 + fromIntegral (i*width) - 3
                  at = translate upperLeft (Coords (-3) right)
              drawAlignedTxt_ (pack colTitle) myDarkGray (mkCentered at)
              ) $ zip [0..] l

progress :: Frame -> Frame -> ColorString
progress (Frame cur) (Frame total) =
  let points = replicate cur '-' ++ replicate (total - cur) ' '
  in  colored' "[" myLightGray <> colored' (pack points) myDarkGray <> colored' "]" myLightGray
