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
  , OldTestProgress
  , mkZeroProgress
  )
  where

import           Imj.Prelude hiding(div)

import           Prelude(putStrLn, putStr, length)
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
import qualified Data.Set as Set
import           Data.String(IsString(..))
import           Data.UUID(UUID)
import           System.Directory(doesFileExist)
import           System.Random.MWC
import           System.IO(stdout, hFlush)
import           Text.Blaze.Html5(div,Html, toHtml)


import           Imj.Game.Hamazed.World.Space.Types
import           Imj.Graphics.Class.Words(Characters)
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
import           Imj.Timing

-- | Represents the progress of 'withTestScheduler''
data TestProgress = TestProgress {
    _uuid :: !UUID
    -- ^ Test unique identifier
  , _timeoutThisIteration :: !(Time Duration System)
  , _hints :: !(Map SmallWorldCharacteristics Int)
   -- ^ Hints for the key of the fastest strategies
  , willTestInIteration :: ![[(SmallWorldCharacteristics, IntMap (Maybe MatrixVariants))]]
   -- ^
   -- The outer list, when filtered on a single ComponentCount, is sorted by /increasing/ areas of the first 'SmallWorldCharacteristics' of the inner list.
   -- During the current iteration, we will skip the elements of the inner list that come after
   -- the first one that timeouts.
  , _willTestNextIteration :: ![[(SmallWorldCharacteristics, IntMap (Maybe MatrixVariants))]]
  -- ^ The outer list is sorted by decreasing areas of the first 'SmallWorldCharacteristics' of the inner list
  -- These are elements of the inner lists that timeouted, or were located after an element that timeouted.
  , _profileResults :: !(MaybeResults (NonEmpty SeedNumber))
} deriving (Generic)
instance Binary TestProgress

data OldTestProgress = OldTestProgress {
    _uuid' :: !UUID
    -- ^ Test unique identifier
  , _timeoutThisIteration' :: !(Time Duration System)
  , _hints' :: !(Map SmallWorldCharacteristics Int)
   -- ^ Hints for the key of the fastest strategies
  , _willTestInIteration' :: ![[(SmallWorldCharacteristics, IntMap (Maybe MatrixVariants))]]
   -- ^
   -- The outer list, when filtered on a single ComponentCount, is sorted by /increasing/ areas of the first 'SmallWorldCharacteristics' of the inner list.
   -- During the current iteration, we will skip the elements of the inner list that come after
   -- the first one that timeouts.
  , _willTestNextIteration' :: ![[(SmallWorldCharacteristics, IntMap (Maybe MatrixVariants))]]
  -- ^ The outer list is sorted by decreasing areas of the first 'SmallWorldCharacteristics' of the inner list
  -- These are elements of the inner lists that timeouted, or were located after an element that timeouted.
  , _profileResults' :: !(OldMaybeResults (NonEmpty SeedNumber))
} deriving (Generic)
instance Binary OldTestProgress

mkZeroProgress :: [[(SmallWorldCharacteristics, IntMap (Maybe MatrixVariants))]]
               -> IO TestProgress
mkZeroProgress l = do
  key <- randUUID
  return $ TestProgress key dt0 mempty l [] (mkNothingResults $ Set.fromList $ concatMap (map fst) l)
 where
  dt0 = fromSecs 0.0001

updateProgress :: Time Duration System
               -> Map SmallWorldCharacteristics Int
               -> [[(SmallWorldCharacteristics, IntMap (Maybe MatrixVariants))]]
               -> [[(SmallWorldCharacteristics, IntMap (Maybe MatrixVariants))]]
               -> MaybeResults (NonEmpty SeedNumber)
               -> TestProgress
               -> TestProgress
updateProgress a b c d e p =
  TestProgress (_uuid p) a b c d e

writeReports :: TestProgress -> UserIntent -> IO ()
writeReports progress@(TestProgress key theDt _ _ _ valids) i = do
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

showStep :: Characters s => TestProgress -> [s]
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

showResults :: (Characters s) => MaybeResults a -> [s]
showResults r =
  prettyShowOptimalStrategies $ toOptimalStrategies r

showRefined :: Maybe MatrixVariants -> Maybe MatrixVariants -> ColorString
showRefined from to =
  CS.colored "Refined from: " green <>
  fromString (show from) <>
  CS.colored " to: " green <>
  fromString (show to)

-- | Historically, this function was introduced when already several hours worth of results
-- were gathered and we needed to change the sorting of worlds.
{-
reset :: TestProgress -> TestProgress
reset (TestProgress a b c l1 l2 d) =
  TestProgress a b c (concatMap s $ l2 ++ l1) [] d
 where
  s [] = []
  s l@(_:_)
    | isJust (find ((== 0.5) . prob) l) && (sortOn prob l == l) = [l] -- list is ascending
    | otherwise = splitL
   where
    (lowp,highp) = partition ((< 0.49) . prob) l
    splitL = [reverse lowp, highp]

  prob (SWCharacteristics _ _ p,_) = p
-}

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
      (return . Just . convert)
    False -> do
      putStrLn $ "File " ++ progressFile ++ " not found."
      return Nothing

convert :: OldTestProgress -> TestProgress
convert (OldTestProgress a b c d e oldR) = TestProgress a b c d e $ convertR oldR
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
withTestScheduler' intent testF initialProgress =
  flip (!!) 0 <$> seedGroups >>= start >>= \finalProgress ->
    readMVar intent >>= writeReports finalProgress
 where
  start firstSeedGroup = do
    mapM_ CS.putStrLn $ showStep initialProgress
    go initialProgress
   where
    go :: TestProgress -> IO TestProgress
    go p@(TestProgress _ !dt hints remaining tooHard results) = do
      onReport (writeReports p) intent
      continue intent >>= bool
        (return p)
        (case remaining of
          [] ->
            if null tooHard
              then
                return p
              else do
                -- reverse to keep the sorting property of 'willTestInIteration' (see doc).
                let newProgress = updateProgress (nextDt dt) hints (reverse tooHard) [] results p
                mapM_ CS.putStrLn $ showStep newProgress
                go newProgress
          []:rest -> go $ p{willTestInIteration = rest}
          smalls@((smallEasy,strategies):smallHards):biggerOrDifferents -> do
            let trySmallsLater = go $ updateProgress dt hints biggerOrDifferents (smalls:tooHard) results p
            if not $ shouldTest smallEasy results
              then do
                putStrLn $ "[skip] " ++ prettyShowSWCharacteristics smallEasy
                trySmallsLater
              else do
                putStrLn $ "[test] " ++ prettyShowSWCharacteristics smallEasy
                let hintsIndicesByDistance = map fst $ sortOn snd $ Map.assocs $ Map.fromListWith min $
                      map (\(w,i) -> (i, smallWorldCharacteristicsDistance smallEasy w)) $ Map.assocs hints
                    hintsByDistance = map (\idx -> (idx, fromMaybe (error "logic") $ IMap.lookup idx strategies)) hintsIndicesByDistance
                    orderedStrategies =
                      hintsByDistance ++
                      IMap.assocs (IMap.withoutKeys strategies $ ISet.fromList hintsIndicesByDistance)
                go' smallEasy (take 8 orderedStrategies) >>= maybe
                  trySmallsLater
                  (\((stratI,strategy),_res) -> do
                      -- TODO use this once we removed the index of the strategy to use the variantspec directly (se need to make
                      -- sure we have all the data available to do refining later : data to compute ordered hints)
                      --let newResults' = addUnrefinedResult smallEasy (fmap toVariantsSpec strategy) (Map.singleton firstSeedGroup _res) results
                      ((refinedI,refined), resultsBySeeds) <- refineWithHint smallEasy testF (stratI,strategy) orderedStrategies
                      let newResults = addRefinedResult smallEasy (fmap toVariantsSpec refined) resultsBySeeds results
                          newHints = Map.insert smallEasy refinedI hints
                      mapM_ CS.putStrLn $ showResults newResults
                      putStrLn $ prettyShowSWCharacteristics smallEasy
                      CS.putStrLn $ showRefined strategy refined
                      go $ updateProgress dt newHints (smallHards:biggerOrDifferents) tooHard newResults p)
           where
            go' _ [] = return Nothing
            go' world (assoc@(_,strategy):otherStrategies) =
              continue intent >>= bool
                (return Nothing)
                (do
                  putStrLn $ "[try 1 group] " ++ show strategy
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
               -- ^ The candidates (the hint can be in this list, but it will be filtered away)
               -> IO ((Int,Maybe MatrixVariants),Map (NonEmpty SeedNumber) (TestStatus Statistics))
               -- ^ The best
refineWithHint world testF hintStrategy strategies = do
  putStrLn "[refine] "
  res <- mkHintStats >>= go (filter (/= hintStrategy) strategies) . mkBestSofar hintStrategy
  putStrLn ""
  return res
 where
  go [] (BestSofar s l _) = return (s, l)
  go (candidate:candidates) bsf@(BestSofar _ _ (DurationConstraints maxTotal maxIndividual)) =
    mkStatsWithConstraint >>= go candidates . either
      (const bsf)
      (mkBestSofar candidate)
   where
    mkStatsWithConstraint :: IO (Either () (Map (NonEmpty SeedNumber) (TestStatus Statistics)))
    mkStatsWithConstraint  = do
      putStr "." >> hFlush stdout
      seedGroups >>= ho [] zeroDuration
     where
      ho l totalSofar remaining
        | totalSofar >= maxTotal = return $ Left ()
        | otherwise = case remaining of
            [] -> do
              putStr "+" >> hFlush stdout
              return $ Right $ Map.fromList l
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
  mkHintStats :: IO (Map (NonEmpty SeedNumber) (TestStatus Statistics))
  mkHintStats =
    seedGroups >>= fmap Map.fromList . mapM
      (\seedGroup -> (,) seedGroup . uncurry mkResultFromStats . fromMaybe (error "logic") <$> -- Nothing is an errorr because we don't specify a timeout
        withNumberedSeeds (testF Nothing $ mkProperties world $ snd hintStrategy) seedGroup)

data BestSofar = BestSofar {
    _strategy :: !(Int, Maybe MatrixVariants)
  , _results :: Map (NonEmpty SeedNumber) (TestStatus Statistics)
  , _inducedContraints :: !DurationConstraints
}

mkBestSofar :: (Int, Maybe MatrixVariants) -> Map (NonEmpty SeedNumber) (TestStatus Statistics) -> BestSofar
mkBestSofar s l = BestSofar s l $ mkDurationConstraints $ Map.elems l
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
