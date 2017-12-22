import           System.Console.ANSI(clearScreen)

import           Control.Monad.Reader(runReaderT)

import           Imj.Draw
import           Imj.Render.Naive

import           Test.Imj.InterpolatedColorString

main :: IO ()
main = do
  putStrLn "" -- for readablilty

  clearScreen -- to not overwrite current terminal content.
  runReaderT (testICS >>
              renderDrawing
              ) (NaiveDraw 2)
