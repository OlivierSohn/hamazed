{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Random.MWC.MkSeeds
       ( writeSeedsSource
       ) where


import           Imj.Prelude

import           Prelude(writeFile)
import           Data.List hiding(intercalate, concat)
import qualified Data.List as List(intercalate, concat)
import qualified Data.Vector.Unboxed as U
import           System.Random.MWC

import           Imj.Util

writeSeedsSource :: IO ()
writeSeedsSource =
  mkSeedsSourceFile 100 >>= writeFile "./imj-profile/src/Imj/Random/MWC/Seeds.hs"

mkSeedsSourceFile :: Int -> IO String
mkSeedsSourceFile n = do
  allSeeds <- (\x -> [" [", unlines $ intersperse " ," x, " ]"]) <$>
    replicateM n (do
      allNums <- unlines . map ((++) "    " . List.concat) . splitEvery 8 . words . List.intercalate ", " . map show <$> mkSeedSystem
      return $ unlines ["  [", allNums, "  ]"])
  return $ unlines $
    [ "-- | The source code for this module was generated by 'mkSeedsSourceFile'"
    , ""
    , "{-# LANGUAGE NoImplicitPrelude #-}"
    , ""
    , "module Imj.Random.MWC.Seeds"
    , "       ( deterministicMWCSeeds"
    , "       ) where"
    , ""
    , "import           Data.Word(Word32)"
    , ""
    , "-- | Provides seeds for MWC RNG, generated at the time the source was written"
    , "--  using 'acquireSeedSystem'."
    , "deterministicMWCSeeds :: [[Word32]]"
    , "deterministicMWCSeeds = "
    ]
    ++ allSeeds
 where
  mkSeedSystem =
    -- drop initial index and carry which are not needed
    reverse . drop 2 . reverse . U.toList . fromSeed <$> (save =<< createSystemRandom)