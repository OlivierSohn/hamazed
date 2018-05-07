{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}

module Imj.Profile.Reports
  ( showStep
  , showResults
  , showRefined
  , writeReports
  , decodeProgressFile
  )
  where

import           Imj.Prelude hiding(div)

import           Prelude(putStrLn, length)
import           Data.Binary
import           Data.String(IsString(..))
import           System.Directory(doesFileExist)
import           Text.Blaze.Html5(div,Html, toHtml)

import           Imj.Graphics.Class.Words(Characters)
import qualified Imj.Graphics.Class.Words as W
import           Imj.Profile.Types
import           Imj.Space.Types

import           Imj.Graphics.Color
import           Imj.Graphics.Text.ColorString (ColorString)
import qualified Imj.Graphics.Text.ColorString as CS
import           Imj.Graphics.Text.Render
import           Imj.Profile.Intent
import           Imj.Profile.Render
import           Imj.Profile.Results
import           Imj.Space.Strategies
import           Imj.Timing

showStep :: Characters s => TestProgress -> [s]
showStep (TestProgress _ theDt _ strategies worldsNow worldsLater _) =
  showArrayN Nothing $ map (map (W.colorize (onBlack yellow) . fromString))
    [ ["Timeout ", showTime theDt]
    , ["Easy candidates", show nEasy]
    , ["Difficult candidates", show nDifficult]
    , ["Later candidates", show nLater]
    , ["Strategies", show $ length strategies]
    ]
 where
   n = sum $ map length worldsNow
   nLater = sum $ map length worldsLater
   nEasy = length worldsNow
   nDifficult = n - nEasy

showResults :: (Characters s) => MaybeResults a -> [s]
showResults r =
  prettyShowOptimalStrategies $ toOptimalStrategies r

showRefined :: Maybe MatrixVariantsSpec -> Maybe MatrixVariantsSpec -> ColorString
showRefined from to =
  CS.colored "Refined from: " green <>
  fromString (show from) <>
  CS.colored " to: " green <>
  fromString (show to)


writeReports :: FilePath
             -- ^ path of the optimal strategies file.
             -> TestProgress -> UserIntent -> IO ()
writeReports path progress@(TestProgress key theDt _ _ _ _ valids) i = do
  let optimalStats = toOptimalStrategies valids
      divArray :: [ColorString] -> Html
      divArray = div . mapM_ (div . toHtml)
      h =
        div $ do
          divArray $ showStep progress
          divArray $ prettyShowOptimalStrategies optimalStats
          resultsToHtml (Just theDt) valids
  writeHtmlReport key h i
  encodeOptimalStrategiesFile path optimalStats
  encodeProgressFile progress

progressFile :: FilePath
progressFile = "testState.bin"

encodeProgressFile :: TestProgress -> IO ()
encodeProgressFile s = do
  encodeFile progressFile s
  putStrLn $ "Wrote test progress file:" ++ show progressFile

decodeProgressFile :: IO (Maybe TestProgress)
decodeProgressFile =
  doesFileExist progressFile >>= bool
    (do
      putStrLn $ "File " ++ progressFile ++ " not found."
      return Nothing)
    (decodeFileOrFail progressFile >>= either
      (\e -> error $ "File " ++ progressFile ++ " seems corrupt: " ++ show e)
      (return . Just))
