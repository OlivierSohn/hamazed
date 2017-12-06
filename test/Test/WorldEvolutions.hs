{-# LANGUAGE OverloadedStrings #-}

module Test.WorldEvolutions(testWE) where


import Game.World.Evolution
import Game.Types
import Game.Render
import Render
import Timing
testWE :: IO ()
testWE = do
  t <- getCurrentTime
  let f1 = FrameSpec (WorldSize (Coords (Row 3) (Col 3))) (RenderState (Coords (Row 3) (Col 3)))
      curInfos = mkInfos Normal        0 [] (Level 1 5 Nothing)
      newInfos = mkInfos ColorAnimated 10 [] (Level 1 5 Nothing)
  print curInfos
  print newInfos
  let
      wa@(WorldAnimation (WorldEvolutions _ _ _) _ _ ) = mkWorldAnimation (f1, curInfos) (f1, newInfos) t

      --wa = mkWorldAnimation (f1, ((["."],["."]),(["."],["."]))) (f1, (([""],[""]),([""],[""]))) t
      --wa = mkWorldAnimation (f1, (([""],[""]),([""],[""]))) (f1, (([""],[""]),([""],[""]))) t
  print wa
