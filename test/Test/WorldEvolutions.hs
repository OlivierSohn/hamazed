
module Test.WorldEvolutions(testWE) where


import Game.World.Evolution
import Game.Types
import Game.Render
import Render
import Render.Console

testWE :: IO ()
testWE = do
  t <- getCurrentTime
  ctxt <- newContext
  let f1 = FrameSpec (WorldSize (Coords (Row 3) (Col 3))) (RenderState (Coords (Row 3) (Col 3)) ctxt)
      curInfos = mkInfos Normal        0 [] (Level 1 5 Nothing)
      newInfos = mkInfos ColorAnimated 10 [] (Level 1 5 Nothing)
  print curInfos
  print newInfos
  let
      wa = mkWorldAnimation (f1, curInfos) (f1, newInfos) t
  print wa
