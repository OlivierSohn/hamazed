{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE OverloadedStrings #-}

{- | Examples of animated text.

Run @imj-base-examples-exe@ to see these examples displayed in the terminal,
in a grid.

Grid lines correspond to different examples, and grid columns are :

* left : using "AnchorChars"
* right: using "StringChars"

-}

module Imj.Example.SequentialTextTranslationsAnchored
    ( exampleOfsequentialTextTranslationsAnchored
    -- * Reexports
    , module Imj.Graphics.Text.Animation
    ) where

import           Data.Text(pack)
import           Data.Semigroup((<>))
import           Control.Concurrent(threadDelay)
import           Control.Monad.IO.Class(MonadIO, liftIO)
import           Control.Monad.Reader.Class(MonadReader)

import           Imj.Geo.Discrete.Types
import           Imj.Graphics.Class.Render
import           Imj.Graphics.Color
import           Imj.Graphics.Render.FromMonadReader
import           Imj.Graphics.Class.Positionable
import           Imj.Graphics.Text.Animation
import           Imj.Graphics.Text.ColorString
import           Imj.Graphics.UI.Colored
import           Imj.Graphics.UI.RectContainer

-- | Shows the differences between 'AnchorChars' and 'AnchorStrings', by comparing,
-- with the same inputs:
--
-- * 'mkSequentialTextTranslationsCharAnchored' / 'drawAnimatedTextCharAnchored'
-- * 'mkSequentialTextTranslationsStringAnchored' / 'drawAnimatedTextStringAnchored'
exampleOfsequentialTextTranslationsAnchored :: (Render e, MonadReader e m, MonadIO m)
                                            => m ()
exampleOfsequentialTextTranslationsAnchored = do
  let (Examples es') = allExamples
      es = accumHeights es' 0
  animate es

accumHeights :: [Example] -> Length Height -> [Example]
accumHeights [] _ = []
accumHeights (Example a h _ b c:es) acc =
              Example a h acc b c:accumHeights es (acc + h)

cellWidth :: Length Width
cellWidth = 30

upperLeft :: Coords Pos
upperLeft = Coords 4 50

newtype Examples = Examples [Example]

data Example = Example {
    _exampleInputData :: ![(Successive ColorString, Coords Pos, Coords Pos)]
  , _exampleSelfHeight :: !(Length Height)
  , _exampleStartHeight :: !(Length Height)
  , _exampleName :: !String
  , _exampleComment :: !String
}

runExampleCharAnchored :: (Render e, MonadReader e m, MonadIO m)
                       => [(Successive ColorString, Coords Pos, Coords Pos)]
                       -> Coords Pos
                       -> (Frame, Frame -> m ())
runExampleCharAnchored input ref =
  let anim@(TextAnimation _ _ (EaseClock (Evolution _ lastFrame _ _))) =
        mkSequentialTextTranslationsCharAnchored (translateInput ref input) (fromSecs 1)
  in (lastFrame, drawAnimatedTextCharAnchored anim)

runExampleStringAnchored :: (Render e, MonadReader e m, MonadIO m)
                         => [(Successive ColorString, Coords Pos, Coords Pos)]
                         -> Coords Pos
                         -> (Frame, Frame -> m ())
runExampleStringAnchored input ref =
  let anim@(TextAnimation _ _ (EaseClock (Evolution _ lastFrame _ _))) =
        mkSequentialTextTranslationsStringAnchored (translateInput ref input) (fromSecs 1)
  in (lastFrame, drawAnimatedTextStringAnchored anim)

translateInput :: Coords Pos
               -> [(Successive ColorString, Coords Pos, Coords Pos)]
               -> [(Successive ColorString, Coords Pos, Coords Pos)]
translateInput tr =
  map (\(l, c1, c2) -> (l, translate tr c1, translate tr c2))

allExamples :: Examples
allExamples =
  Examples
    [ Example exampleDownTranslationDuo 7 0
                    "DownTranslationDuo"
                    "Char and String anchors give different results because there is a move."
    , Example exampleDownTranslationMono 7 0
                    "DownTranslationMono"
                    "Char and String anchors give different results because there is a move."
    , Example exampleIntermediateCharAdditions 6 0
                    "IntermediateCharAdditions"
                    $  "When the chars are inserted in the middle, their color "
                    ++ "is a gradual interpolation between neighbour colors."
    , Example exampleIntermediateCharRemovals 6 0
                    "IntermediateCharRemovals"
                    ""
    , Example exampleExtremeCharAdditions 6 0
                    "ExtremeCharAdditions"
                    $  "When the chars are inserted at an extremity,"
                    ++ " they match the neighbour color."
    , Example exampleExtremeCharRemovals 6 0
                    "ExtremeCharRemovals"
                    ""
    , Example exampleCharRemovalsAndReplacements 6 0
                    "CharRemovalsAndReplacements"
                    "Here we should (todo) shrink before replacing."
    ]

-- | shows an example with multiple strings : global color is changed in parallel
-- but anchors are changed sequentially
exampleDownTranslationDuo :: [(Successive ColorString, Coords Pos, Coords Pos)]
exampleDownTranslationDuo =
  let a = colored "ABC" green <> colored "DEF" (rgb 2 2 2)
      b = colored "ABC" white <> colored "DEF" yellow
      txt1 = Successive [a, b]
      from1 = Coords 0 0
      to1 = Coords 1 0
      from2 = Coords 0 10
      to2 = Coords 1 10
  in [(txt1, from1, to1)
    , (txt1, from2, to2)]

exampleDownTranslationMono :: [(Successive ColorString, Coords Pos, Coords Pos)]
exampleDownTranslationMono =
  let a = colored "ABC" green <> colored "DEF" (rgb 2 2 2)
      b = colored "ABC" white <> colored "DEF" yellow
      txt1 = Successive [a, b]
      from1 = Coords 0 0
      to1 = Coords 1 0
  in [(txt1, from1, to1)]


exampleIntermediateCharAdditions :: [(Successive ColorString, Coords Pos, Coords Pos)]
exampleIntermediateCharAdditions =
  let a = colored "A" green <> colored "O" white
      b = colored "ABCDEFGHIJKLMNO" white
      txt1 = Successive [a, b]
      from1 = Coords 0 0
      to1 = Coords 0 0
  in [(txt1, from1, to1)]

exampleIntermediateCharRemovals :: [(Successive ColorString, Coords Pos, Coords Pos)]
exampleIntermediateCharRemovals =
  let a = colored "ABCDEFGHIJKLMNO" white
      b = colored "A" green <> colored "O" white
      txt1 = Successive [a, b]
      from1 = Coords 0 0
      to1 = Coords 0 0
  in [(txt1, from1, to1)]

exampleExtremeCharAdditions :: [(Successive ColorString, Coords Pos, Coords Pos)]
exampleExtremeCharAdditions =
  let a = colored "ABC" green
      b = colored "ABC" green <> colored "DEF" red
      txt1 = Successive [a, b]
      from1 = Coords 0 0
      to1 = Coords 0 0
  in [(txt1, from1, to1)]


exampleExtremeCharRemovals :: [(Successive ColorString, Coords Pos, Coords Pos)]
exampleExtremeCharRemovals =
  let a = colored "ABC" green <> colored "DEF" white
      b = colored "ABC" green
      txt1 = Successive [a, b]
      from1 = Coords 0 0
      to1 = Coords 0 0
  in [(txt1, from1, to1)]


exampleCharRemovalsAndReplacements :: [(Successive ColorString, Coords Pos, Coords Pos)]
exampleCharRemovalsAndReplacements =
  let a = colored "In" green <> colored "stalla" yellow <> colored "tion" green
      b = colored "In" green <> colored "fla" yellow <> colored "tion" green
      txt1 = Successive [a, b]
      from1 = Coords 0 0
      to1 = Coords 0 0
  in [(txt1, from1, to1)]

animate :: (Render e, MonadReader e m, MonadIO m)
        => [Example]
        -> m ()
animate examples = do
  let listActions = concatMap (\ex@(Example e _ startHeight _ _) ->
                    let (a,b) = runExampleCharAnchored e ref
                        (c,d) = runExampleStringAnchored e (move (fromIntegral cellWidth) RIGHT ref)
                        ref = getRef startHeight
                    in [(a,b,ex,0),(c,d,ex,1)]) examples
      frames = replicate (length listActions) (Frame 0)
      colTitles = ["Char anchored", "String anchored"]
  animate' listActions examples frames colTitles

getRef :: Length Height -> Coords Pos
getRef startHeight =
  translate upperLeft $ Coords (fromIntegral startHeight) 0

animate' :: (Render e, MonadReader e m, MonadIO m)
        => [(Frame, Frame -> m (), Example, Int)]
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
  _ <- renderToScreen
  liftIO $ threadDelay 1000000
  animate' listActions examples newFrames colTitles

myDarkGray :: LayeredColor
myDarkGray = onBlack $ gray 6
myLightGray :: LayeredColor
myLightGray = onBlack $ gray 10

drawActions :: (Render e, MonadReader e m, MonadIO m)
            => [(Frame, Frame -> m (), Example, Int)]
            -> [Frame]
            -> m ()
drawActions listActions frames =
  mapM_ (\(frame, (lastFrame, action, Example _ h startHeight _ _, wIdx)) -> do
            let r = RectContainer (Size (h-2) (cellWidth-3)) (translate (Coords (-2) (-2)) ref)
                ref = move (wIdx * fromIntegral cellWidth) RIGHT (getRef startHeight)
            drawUsingColor r myDarkGray
            action frame
            drawAt (progress frame lastFrame) (translate ref $ Coords (fromIntegral h - 4) 0)
            ) $ zip frames listActions

drawExamples :: (Render e, MonadReader e m, MonadIO m)
             => [Example]
             -> m ()
drawExamples =
  mapM_ drawOneExample
 where
  drawOneExample (Example _ _ startHeight leftTitle rightComment) = do
    let down = fromIntegral startHeight
        at = translate upperLeft (Coords down (-8))
        at' = translate upperLeft (Coords down $ 2 * fromIntegral cellWidth)
    drawAligned_ (Colored myDarkGray leftTitle)  (mkRightAlign at)
    drawMultiLineStr rightComment at' myDarkGray 30

drawColTitles :: (Render e, MonadReader e m, MonadIO m)
            => [String]
            -> m ()
drawColTitles l =
    mapM_ (\(i,colTitle) -> do
              let right = quot (fromIntegral cellWidth) 2 + fromIntegral (i*cellWidth) - 3
                  at = translate upperLeft (Coords (-3) right)
              drawAligned_ (Colored myDarkGray colTitle) (mkCentered at)
              ) $ zip [0..] l

progress :: Frame -> Frame -> ColorString
progress (Frame cur) (Frame total) =
  let points = replicate cur '-' ++ replicate (total - cur) ' '
  in  colored' "[" myLightGray <> colored' (pack points) myDarkGray <> colored' "]" myLightGray
