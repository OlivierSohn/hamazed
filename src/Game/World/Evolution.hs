{-# LANGUAGE NoImplicitPrelude #-}

module Game.World.Evolution
           ( evolveAt
           , renderEvolutions
           ) where

import           Imajuscule.Prelude

import           Game.World.Types

import           Game.World.Frame

-- evolveAt, renderEvolutions share some logic:
-- TODO make a function that does the traversal of Evolutions with substraction of the last frame.

renderEvolutions :: WorldEvolutions -> Frame -> IO ()
renderEvolutions
 (WorldEvolutions frameE@(Evolution _ lastFrameFrameE _ _) upDown@(TextAnimation _ (Evolution _ lastFrameUD _ _)) left)
 frame = do
  let relFrameFrameE = max 0 frame
      relFrameUD = max 0 (relFrameFrameE - lastFrameFrameE)
      relFrameLeft = max 0 (relFrameUD - lastFrameUD)
  renderWorldFrame frameE relFrameFrameE
  renderAnimatedTextCharAnchored upDown relFrameUD
  renderAnimatedTextStringAnchored left relFrameLeft

evolveAt :: Frame -> WorldEvolutions -> Maybe Float
evolveAt frame (WorldEvolutions frameE@(Evolution _ lastFrameFrameE _ _) (TextAnimation _ upDown@(Evolution _ lastFrameUD _ _)) (TextAnimation _ center)) =
  let relFrameFrameE = max 0 frame
      relFrameUD = max 0 (relFrameFrameE - lastFrameFrameE)
      relFrameLeft = max 0 (relFrameUD - lastFrameUD)
  in evolveDeltaTime frameE relFrameFrameE
      <|> evolveDeltaTime upDown relFrameUD
      <|> evolveDeltaTime center relFrameLeft
