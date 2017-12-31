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
  let (Examples es') = allExamples
      es = accumHeights es' 0
  animate es

accumHeights :: [Example] -> Length Height -> [Example]
accumHeights [] _ = []
accumHeights ((Example a h _ b c):es) acc =
              (Example a h acc b c):accumHeights es (acc + h)

width :: Length Width
width = 30

upperLeft :: Coords Pos
upperLeft = Coords 4 50

data Examples = Examples [Example]

data Example = Example {
    _exampleInputData :: ![([ColorString], Coords Pos, Coords Pos)]
  , _exampleSelfHeight :: !(Length Height)
  , _exampleStartHeight :: !(Length Height)
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
    [ Example exampleColorComplexWithMotionAndTwoStrings 7 0
                    "ColorComplexWithMotionAndTwoStrings"
                    "Char and String anchors give different results because there is a move"
    , Example exampleColorComplexWithMotion 7 0
                    "ColorComplexWithMotion"
                    "Char and String anchors give different results because there is a move"
    , Example exampleCharAdditionsAndChanges 10 0
                    "CharAdditionsAndChanges"
                    "We first interpolate characters, then the colors. Anchors are interpolated at the same time."
    , Example exampleIntermediateCharAdditions 6 0
                    "IntermediateCharAdditions"
                    "When the chars are inserted in the middle, their color is a gradual interpolation between neighbour colors."
    , Example exampleExtremeCharAdditions 6 0
                    "ExtremeCharAdditions"
                    "When the chars are inserted at an extremity, they match the neighbour color."
    , Example exampleOneCharChange 6 0
                    "OneCharChange"
                    "When the char changes, the old color is kept at first"
    , Example exampleColorComplex 6 0
                    "ColorComplex"
                    "Char and String anchors give the same result because there is no move"
    , Example exampleColor 6 0
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

exampleCharAdditionsAndChanges :: [([ColorString], Coords Pos, Coords Pos)]
exampleCharAdditionsAndChanges =
  let a = colored "ABC" green
      b = colored "DEFGH" blue
      txt1 = [a, b]
      from1 = Coords 0 0
      to1 = Coords 4 0
  in [(txt1, from1, to1)]


exampleIntermediateCharAdditions :: [([ColorString], Coords Pos, Coords Pos)]
exampleIntermediateCharAdditions =
  let a = colored "A" green <> colored "O" white
      b = colored "A" green <> colored "BCDEFGHIJKLMN" white <> colored "O" green
      txt1 = [a, b]
      from1 = Coords 0 0
      to1 = Coords 0 0
  in [(txt1, from1, to1)]

exampleExtremeCharAdditions :: [([ColorString], Coords Pos, Coords Pos)]
exampleExtremeCharAdditions =
  let a = colored "ABC" green
      b = colored "ABC" green <> colored "DEF" white
      txt1 = [a, b]
      from1 = Coords 0 0
      to1 = Coords 0 0
  in [(txt1, from1, to1)]

exampleOneCharChange :: [([ColorString], Coords Pos, Coords Pos)]
exampleOneCharChange =
  let a = colored "A" green <> colored "B" white <> colored "C" green
      b = colored "AFC" green
      txt1 = [a, b]
      from1 = Coords 0 0
      to1 = Coords 0 0
  in [(txt1, from1, to1)]

animate :: (Render e, MonadReader e m, MonadIO m)
        => [Example]
        -> m ()
animate examples = do
  let listActions = concatMap (\ex@(Example e _ startHeight _ _) ->
                    let (a,b) = runExampleCharAnchored e ref
                        (c,d) = runExampleStringAnchored e (move (fromIntegral width) RIGHT ref)
                        ref = getRef startHeight
                    in [(a,b,ex,0),(c,d,ex,1)]) examples
      frames = replicate (length listActions) (Frame 0)
      colTitles = ["Char anchored", "String anchored"]
  animate' listActions examples frames colTitles

getRef :: Length Height -> Coords Pos
getRef startHeight =
  translate upperLeft $ Coords (fromIntegral startHeight) 0

animate' :: (Render e, MonadReader e m, MonadIO m)
        => [(Frame, (Frame -> m ()), Example, Int)]
        -> [Example]
        -> [Frame]
        -> [String]
        -> m ()
animate' listActions examples frames colTitles = do
  let newFrames = zipWith (\count (lastFrame, _, _, _) ->
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
            => [(Frame, (Frame -> m ()), Example, Int)]
            -> [Frame]
            -> m ()
drawActions listActions frames =
  mapM_ (\(frame, (lastFrame, action, (Example _ height startHeight _ _), wIdx)) -> do
            let r = RectContainer (Size (height-2) (width-3)) (translate (Coords (-2) (-2)) ref)
                ref = move (wIdx * fromIntegral width) RIGHT (getRef startHeight)
            drawUsingColor r myDarkGray
            action frame
            drawColorStr (progress frame lastFrame) (translate ref $ Coords (fromIntegral height - 4) 0)
            ) $ zip frames listActions

drawExamples :: (Render e, MonadReader e m, MonadIO m)
            => [Example]
            -> m ()
drawExamples examples =
  mapM_ (\(Example _ height startHeight leftTitle rightComment) -> do
            let down = (quot (fromIntegral height) 2) + fromIntegral startHeight - 2
                at = translate upperLeft (Coords down (-8))
                at' = translate upperLeft (Coords down $ 2 * (fromIntegral width))
            drawAlignedTxt_ (pack leftTitle) myDarkGray (mkRightAlign at)
            drawStr rightComment at' myDarkGray
            ) examples

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
