
import Game.World.Space
import Game.World.Size
import Math
import Render.Console

main :: IO ()
main = testSpace

testSpace :: IO()
testSpace = do
  let blocksSize = 6
      ws = worldSizeFromLevel 1 Rectangle2x1
  s <- mkRandomlyFilledSpace (RandomParameters blocksSize StrictlyOneComponent) (WorldSize $ Coords (Row 36) (Col 72))
  beginFrame
  renderSpace s $ RenderState (Coords (Row 0) (Col 0))
  endFrame
  return ()

testEase :: IO()
testEase = do
  putStrLn ""
  test invQuartEaseInOut
  putStrLn ""
  test quartInOut

test :: (Float -> Float) -> IO ()
test f = mapM_ (\v -> putStrLn $ show v) $ map f $ map (\i -> fromIntegral i / 10.0) [0..10]

quartInOut :: Float -> Float
quartInOut time =
    if time < 0.5
    then        1 / 2 *  2^4 * time  * time  * time  * time
    else negate 1 / 2 * (2^4 * (time-1) * (time-1) * (time-1) * (time-1) - 2)
