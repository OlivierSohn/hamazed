{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}

module Imj.Profile.Scheduler
  ( withTestScheduler'
  , decodeProgressFile
  , mkResultFromStats
  , continue
  , TestProgress
  , mkZeroProgress
  )
  where

import           Imj.Prelude hiding(div)

import           Prelude(putStrLn, length)
import           Control.Concurrent(threadDelay)
import           Control.Concurrent.MVar.Strict (MVar, readMVar)
import           Data.Binary
import           Data.List.NonEmpty (NonEmpty(..))
import           Data.List as List
import           Data.IntMap.Strict(IntMap)
import qualified Data.IntMap.Strict as IMap
import qualified Data.IntSet as ISet
import           Data.Map.Strict(Map)
import qualified Data.Map.Strict as Map
import           Data.String(IsString(..))
import           Data.Text(pack)
import           Data.UUID(UUID)
import           System.Directory(doesFileExist)
import           System.Random.MWC
import           Text.Blaze.Html5(div,Html, toHtml)

import           Imj.Game.Hamazed.World.Space.Types
import qualified Imj.Graphics.Class.Words as W

import           Imj.Game.Hamazed.World.Space.Strategies
import           Imj.Graphics.Color
import           Imj.Graphics.Text.ColorString (ColorString)
import qualified Imj.Graphics.Text.ColorString as CS
import           Imj.Graphics.Text.Render
import           Imj.Profile.Intent
import           Imj.Profile.Render
import           Imj.Profile.Result
import           Imj.Profile.Results
import           Imj.Random.MWC.Util
import           Imj.Random.Util
import           Imj.Timing

-- | Represents the progress of 'withTestScheduler''
data TestProgress = TestProgress {
    _uuid :: !UUID
    -- ^ Test unique identifier
  , _timeoutThisIteration :: !(Time Duration System)
  , _hints :: !(Map SmallWorldCharacteristics Int)
   -- ^ Hints for the key of the fastest strategies
  , willTestInIteration :: ![[(SmallWorldCharacteristics, IntMap (Maybe MatrixVariants))]]
   -- ^ During the current iteration, we test the first element of each inner list.
   -- The elements thereafter are considered too difficult and will be tested at a later iteration.
  , _willTestNextIteration :: ![[(SmallWorldCharacteristics, IntMap (Maybe MatrixVariants))]]
  -- ^ These will be tested in a later iteration
  , _profileResults :: !(Results (NonEmpty SeedNumber))
} deriving (Generic)
instance Binary TestProgress

mkZeroProgress :: [[(SmallWorldCharacteristics, IntMap (Maybe MatrixVariants))]]
               -> IO TestProgress
mkZeroProgress l = do
  key <- randUUID
  return $ TestProgress key dt0 mempty l [] mkEmptyResults
 where
  dt0 = fromSecs 0.0001

updateProgress :: Time Duration System
               -> Map SmallWorldCharacteristics Int
               -> [[(SmallWorldCharacteristics, IntMap (Maybe MatrixVariants))]]
               -> [[(SmallWorldCharacteristics, IntMap (Maybe MatrixVariants))]]
               -> Results (NonEmpty SeedNumber)
               -> TestProgress
               -> TestProgress
updateProgress a b c d e p =
  TestProgress (_uuid p) a b c d e

progressFile :: FilePath
progressFile = "testState.bin"

encodeProgressFile :: TestProgress -> IO ()
encodeProgressFile s = do
  encodeFile progressFile s
  putStrLn $ "Wrote test progress file:" ++ show progressFile

decodeProgressFile :: IO (Maybe TestProgress)
decodeProgressFile =
  doesFileExist progressFile >>= \case
    True -> decodeFileOrFail progressFile >>= either
      (\e -> error $ "File " ++ progressFile ++ " seems corrupt: " ++ show e)
      (return . Just)
    False -> do
      putStrLn $ "File " ++ progressFile ++ " not found."
      return Nothing

-- |
-- For each world:
--   Using a single seed group, find /a/ strategy that works within a given time upper bound.
--   Using all seed groups, and the found strategy as a hint, find the best strategy.
--   Accumulate the best strategies in a list, and test them first in subsequent iterations.
withTestScheduler' :: MVar UserIntent
                   -> (Maybe (Time Duration System) -> Properties -> NonEmpty GenIO -> IO (Maybe (MkSpaceResult SmallWorld, Statistics)))
                   -- ^ Function to test
                   -> TestProgress
                   -> IO ()
withTestScheduler' intent testF initialProgress@(TestProgress key _ _ _ _ _) =
  flip (!!) 0 <$> seedGroups >>= start >>= \finalProgress ->
    readMVar intent >>= report finalProgress
 where
  start firstSeedGroup = do
    informStep initialProgress
    go initialProgress
   where
    go :: TestProgress
       -> IO TestProgress
    go p@(TestProgress _ !dt hints remaining noValidStrategy oneValidStrategy) = do
      onReport (report p) intent
      continue intent >>= bool
        (return p)
        (case remaining of
          [] ->
            if null noValidStrategy
              then
                return p
              else do
                let newProgress = updateProgress (nextDt dt) hints noValidStrategy [] oneValidStrategy p
                informStep newProgress
                go newProgress
          []:rest -> go $ p{willTestInIteration = rest}
          l@((easy,strategies):difficults):nextWorlds -> do
            let hintsIndicesByDistance = map fst $ sortOn snd $ Map.assocs $ Map.fromListWith min $
                  map (\(w,i) -> (i, smallWorldCharacteristicsDistance easy w)) $ Map.assocs hints
                hintsByDistance = map (\idx -> (idx, fromMaybe (error "logic") $ IMap.lookup idx strategies)) hintsIndicesByDistance
                orderedStrategies =
                  hintsByDistance ++
                  IMap.assocs (IMap.withoutKeys strategies $ ISet.fromList hintsIndicesByDistance)
            go' easy orderedStrategies >>= maybe
              (go $ updateProgress dt hints nextWorlds (l:noValidStrategy) oneValidStrategy p)
              (\((stratI,strategy),_) -> do
                  ((refinedI,refined), resultsBySeeds) <- refineWithHint easy testF (stratI,strategy)
                    $ IMap.assocs $ IMap.delete stratI strategies
                  let newOneValid = foldl' (flip (uncurry (addResult easy refined))) oneValidStrategy resultsBySeeds
                      newHints = Map.insert easy refinedI hints
                  showValids newOneValid
                  informRefined strategy refined
                  go $ updateProgress dt newHints (difficults:nextWorlds) noValidStrategy newOneValid p)
           where
            go' _ [] = return Nothing
            go' world (assoc@(_,strategy):otherStrategies) =
              continue intent >>= bool
                (return Nothing)
                (--putStrLn $ "try " ++ show strategy
                  fmap (uncurry mkResultFromStats) <$> withNumberedSeeds (testF (Just dt) (mkProperties world strategy)) firstSeedGroup
                     >>= maybe
                      (go' world otherStrategies)
                      (\case
                        Timeout -> error "logic"
                        NotStarted -> error "logic"
                        f@(Finished dt' _) -> bool
                          (onTimeoutMismatch dt dt' >> go' world otherStrategies)
                          (return $ Just (assoc,f))
                          $ dt' <= dt)))
     where
      showValids valids = do
        let optimalStats = toOptimalStrategies valids
            str = prettyShowOptimalStrategies optimalStats
        mapM_ CS.putStrLn str -- to give a feedback of the progress in the console

  report progress@(TestProgress _ theDt _ _ _ valids) i = do
    let optimalStats = toOptimalStrategies valids
        divArray :: [ColorString] -> Html
        divArray = div . mapM_ (div . toHtml)
        h =
          div $ do
            divArray $ showStep progress
            divArray $ prettyShowOptimalStrategies optimalStats
            resultsToHtml (Just theDt) valids
    writeHtmlReport key h i
    encodeOptimalStrategiesFile optimalStats
    encodeProgressFile progress

  informStep = mapM_ CS.putStrLn . showStep

  showStep (TestProgress _ theDt _ worldsNow worldsLater _) =
    showArrayN Nothing $ map (map (W.colorize (onBlack yellow) . fromString))
      [ ["Timeout ", showTime theDt]
      , ["Easy candidates", show nEasy]
      , ["Difficult candidates", show nDifficult]
      , ["Later candidates", show nLater]
      ]
   where
     n = sum $ map length worldsNow
     nLater = sum $ map length worldsLater
     nEasy = length worldsNow
     nDifficult = n - nEasy

  informRefined :: Maybe MatrixVariants -> Maybe MatrixVariants -> IO ()
  informRefined from to =
    CS.putStrLn $ CS.colored ("Refined from: " <> pack (show from) <> " to: " <> pack (show to)) green

  nextDt = (.*) multDt
  multDt = 6

onTimeoutMismatch :: Time Duration System -> Time Duration System -> IO ()
onTimeoutMismatch specified actual
  | actual <= specified = error "logic"
  | actual <= 2 .* specified = return ()
  | otherwise = putStrLn $ unwords ["Big timeout mismatch:", showTime specified, showTime actual]

continue :: MVar UserIntent -> IO Bool
continue intent = readMVar intent >>= \case
  Cancel -> return False
  Pause _ -> do
    CS.putStrLn $ CS.colored "Test is paused, press 'Space' to continue..." yellow
    let waitForNewIntent = do
          threadDelay 100000
          readMVar intent >>= \case
            Pause _ -> waitForNewIntent
            _ -> return ()
    waitForNewIntent
    continue intent
  _ -> return True

-- note that even in case of timeout we could provide some stats.
mkResultFromStats :: MkSpaceResult a -> Statistics -> TestStatus Statistics
mkResultFromStats res stats = case res of
  NeedMoreTime -> Timeout
  Impossible err -> error $ "impossible :" ++ show err
  Success _ -> Finished (totalDuration $ durations stats) stats


-- | Given a hint variant, computes the best variant, taking the time of the hint
-- as a reference to early-discard others.
refineWithHint :: SmallWorldCharacteristics
               -> (Maybe (Time Duration System) -> Properties -> NonEmpty GenIO -> IO (Maybe (MkSpaceResult a, Statistics)))
               -> (Int,Maybe MatrixVariants)
               -- ^ This is the hint : a variant which we think is one of the fastest.
               -> [(Int,Maybe MatrixVariants)]
               -- ^ The candidates (the hint should not be in this list)
               -> IO ((Int,Maybe MatrixVariants),[(NonEmpty SeedNumber, TestStatus Statistics)])
               -- ^ The best
refineWithHint world testF hintStrategy strategies =
  mkHintStats >>= go strategies . mkBestSofar hintStrategy
 where
  go [] (BestSofar s l _) = return (s, l)
  go (candidate:candidates) bsf@(BestSofar _ _ (DurationConstraints maxTotal maxIndividual)) =
    mkStatsWithConstraint >>= go candidates . either
      (const bsf)
      (mkBestSofar candidate)
   where
    mkStatsWithConstraint :: IO (Either () [(NonEmpty SeedNumber, TestStatus Statistics)])
    mkStatsWithConstraint  =
      seedGroups >>= ho [] zeroDuration
     where
      ho l totalSofar remaining
        | totalSofar >= maxTotal = return $ Left ()
        | otherwise = case remaining of
            [] -> return $ Right l
            seedGroup:groups -> do
              let maxDt = min maxIndividual $ maxTotal |-| totalSofar
                  props = mkProperties world $ snd candidate
              fmap (uncurry mkResultFromStats) <$> withNumberedSeeds (testF (Just maxDt) props) seedGroup >>= maybe
                (return $ Left ())
                (\case
                  Timeout -> error "logic"
                  NotStarted -> error "logic"
                  f@(Finished dt _) ->
                    bool
                      (onTimeoutMismatch maxDt dt >> return (Left ()))
                      (ho ((seedGroup,f):l) (totalSofar |+| dt) groups)
                      $ dt <= maxDt)
  mkHintStats :: IO [(NonEmpty SeedNumber, TestStatus Statistics)]
  mkHintStats =
    seedGroups >>= mapM
      (\seedGroup -> (,) seedGroup . uncurry mkResultFromStats . fromMaybe (error "logic") <$> -- Nothing is an errorr because we don't specify a timeout
        withNumberedSeeds (testF Nothing $ mkProperties world $ snd hintStrategy) seedGroup)

data BestSofar = BestSofar {
    _strategy :: !(Int, Maybe MatrixVariants)
  , _results :: [(NonEmpty SeedNumber, TestStatus Statistics)]
  , _inducedContraints :: !DurationConstraints
}

mkBestSofar :: (Int, Maybe MatrixVariants) -> [(NonEmpty SeedNumber, TestStatus Statistics)] -> BestSofar
mkBestSofar s l = BestSofar s l $ mkDurationConstraints $ map snd l
 where
  mkDurationConstraints :: [TestStatus Statistics]
                        -> DurationConstraints
  mkDurationConstraints results =
    DurationConstraints sumTime $ 3 .* maxTime
   where
    sumTime = foldl' (\t r -> getTime r |+| t) zeroDuration results
    maxTime = foldl' (\t r -> max (getTime r) t) zeroDuration results

    getTime = \case
      Timeout -> error "logic"
      NotStarted -> error "logic"
      Finished dt _ -> dt

data DurationConstraints = DurationConstraints {
    _maxTotal :: !(Time Duration System)
  , _maxIndividual :: !(Time Duration System)
}
