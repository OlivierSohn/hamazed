import System.Console.ANSI(clearScreen)

import Control.Monad.Reader(runReaderT)

import Draw

import Test.InterpolatedColorString
import Render.Naive

main :: IO ()
main = do
  putStrLn "" -- for readablilty

  clearScreen -- to not overwrite current terminal content.
  runReaderT (testICS >>
              renderDrawing
              ) (NaiveDraw 2)
