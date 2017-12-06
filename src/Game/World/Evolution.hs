{-# LANGUAGE NoImplicitPrelude #-}

module Game.World.Evolution
           ( evolveAt
           , renderEvolutions
           ) where

import           Imajuscule.Prelude

import           Game.World.Types

import           Game.World.Frame

renderEvolutions :: WorldEvolutions -> Frame -> IO ()
renderEvolutions
 we@(WorldEvolutions frameE upDown left)
 frame = do
  let (relFrameFrameE, relFrameUD, relFrameLeft) = getFrames we frame
  renderWorldFrame frameE relFrameFrameE
  renderAnimatedTextCharAnchored upDown relFrameUD
  renderAnimatedTextStringAnchored left relFrameLeft

evolveAt :: WorldEvolutions -> Frame -> Maybe Float
evolveAt we@(WorldEvolutions frameE (TextAnimation _ upDown) (TextAnimation _ left)) frame =
  let (relFrameFrameE, relFrameUD, relFrameLeft) = getFrames we frame
  in evolveDeltaTime frameE relFrameFrameE
    <|> evolveDeltaTime upDown relFrameUD
    <|> evolveDeltaTime left relFrameLeft

getFrames :: WorldEvolutions -> Frame -> (Frame, Frame, Frame)
getFrames (WorldEvolutions (Evolution _ lastFrameFrameE _ _)
                           (TextAnimation _ (Evolution _ lastFrameUD _ _)) _) frame =
  let relFrameFrameE = max 0 frame
      relFrameUD = max 0 (relFrameFrameE - lastFrameFrameE)
      relFrameLeft = max 0 (relFrameUD - lastFrameUD)
  in (relFrameFrameE, relFrameUD, relFrameLeft)
