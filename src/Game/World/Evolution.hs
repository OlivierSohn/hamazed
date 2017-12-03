{-# LANGUAGE NoImplicitPrelude #-}

module Game.World.Evolution
           ( evolveAt
           , renderEvolutions
           ) where

import           Imajuscule.Prelude

import           Game.World.Types

-- evolveAt, renderEvolutions share some logic:
-- TODO make a function that does the traversal of Evolutions with substraction of the last frame.

renderEvolutions :: WorldEvolutions -> Frame -> IO ()
renderEvolutions
 (WorldEvolutions upDown@(TextAnimation _ (Evolution _ _ lastFrameUD _ _)) left)
 frame = do
  let relFrame = max 0 (frame - lastFrameUD)
  renderAnimatedText upDown frame
  renderAnimatedText left relFrame

evolveAt :: Frame -> WorldEvolutions -> Maybe Float
evolveAt frame (WorldEvolutions (TextAnimation _ upDown@(Evolution _ _ lastFrameUD _ _)) (TextAnimation _ center)) =
  snd (evolve upDown frame) <|> snd (evolve center (frame-lastFrameUD))
