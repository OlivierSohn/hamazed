module Main where

import           Imj.Prelude

import           Options.Applicative

import           Imj.Measure.Stdout


otherTestsParser :: Parser Settings
otherTestsParser = OtherTests
      <$> switch
          ( long "modes"
          <> short 'm'
          <> help "Test various buffering modes")

testSizesParser :: Parser Settings
testSizesParser = LengthTest
      <$> option auto
          ( long "size"
         <> short 's'
         <> help ("Tests the maximum capacity of stdout. The size value is expected to be >= 2. "
                  ++ "Looking at your terminal's output, when "
                  ++ "you see 1, then 2, then 3, it means : size > stdout maximum size. "
                  ++ "Else if you see 1, then '2 and 3 at the same time': size < stdout maximum size. "
                 )
         <> metavar "INT" )

main :: IO ()
main =
  testStdout =<< customExecParser p opts
  where
    opts = info ((testSizesParser <|> otherTestsParser) <**> helper)
      ( fullDesc
     <> progDesc "imj-measure-stdout-exe can measure the size of stdout." )
    p = prefs showHelpOnError
