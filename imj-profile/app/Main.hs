{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE BangPatterns #-}

module Main where

import           Imj.Prelude

import           Prelude(print, putStrLn, length, writeFile, getChar)
import           Control.Concurrent(forkIO, throwTo, ThreadId, myThreadId, threadDelay)
import           Control.Concurrent.MVar.Strict (MVar, newMVar, modifyMVar_, readMVar)
import           Control.Exception(Exception(..))
import           Control.DeepSeq(NFData(..))
import           Data.List.NonEmpty (NonEmpty(..))
import           Data.List as List hiding(intercalate, concat)
import qualified Data.List as List(intercalate, concat)
import           Data.IntMap.Strict(IntMap)
import qualified Data.IntMap.Strict as IMap
import           Data.IntSet(IntSet)
import qualified Data.IntSet as ISet
import           Data.Map.Strict(Map)
import qualified Data.Map.Strict as Map
import           Data.String(IsString(..))
import           Data.Text(pack)
import           Data.Vector(fromList)
import qualified Data.Vector.Unboxed as U
import           Data.Word(Word32)
import           Foreign.C.Types(CInt)
import           System.Posix.Signals (installHandler, Handler(..), sigINT, sigTERM)
import           System.Random.MWC
import           System.Timeout(timeout)
import           System.IO(hSetBuffering, stdin, BufferMode(..))

import           Imj.Game.Hamazed.World.Space.Types
import qualified Imj.Graphics.Class.Words as W

import qualified Imj.Data.Matrix.Cyclic as Cyclic
import           Imj.Game.Hamazed.World.Space
import           Imj.Graphics.Color
import           Imj.Graphics.Text.ColorString(ColorString)
import qualified Imj.Graphics.Text.ColorString as CS
import           Imj.Graphics.Text.Render
import           Imj.Profile.Render.Characters
import           Imj.Profile.Render
import           Imj.Profile.Result
import           Imj.Timing
import           Imj.Util

import           Imj.Random.MWC.Seeds


-- commands used to profile:
--
-- for cost centers:
-- stack build --executable-profiling --library-profiling --ghc-options="-fprof-auto -rtsopts" && stack exec -- imj-profile +RTS -M2G -p
--
-- for ticky-ticky : add ticky option at compile time (in the module, or in stack.yaml) +
-- stack build && stack exec -- imj-profile-exe +RTS -rprofile.ticky

main :: IO ()
main =
  profileLargeWorld -- simple benchmark, used as ref for benchmarking a new algo
  --profileAllProps -- exhaustive benchmark, to study how to tune strategy wrt world parameters
  --profileAllProps2 -- exhaustive benchmark, with notion of easy / hard test to reach approximated results as fast as possible.
  --writeSeedsSource

justVariantsWithRotations :: Size -> ComponentCount -> [MatrixVariants]
justVariantsWithRotations sz n =
  let interleave = mkInterleaveVariation sz
      modulation = mkModulateVariation sz
  in concatMap (map (\y -> y Nothing)) $
      map (\rotationOrder ->
        let rotation = Rotate $ RotationDetail rotationOrder n
        in  Variants (pure interleave) . Just . Variants (pure modulation) . Just . Variants (pure rotation):
            Variants (modulation :| [interleave]) . Just . Variants (pure rotation):
            Variants (pure interleave) . Just . Variants (pure rotation):
            Variants (pure modulation) . Just . Variants (pure rotation):
            Variants (interleave :| [rotation]):
            Variants (modulation :| [rotation]):
            Variants (interleave :| [modulation,rotation]):
            Variants (pure rotation):
            [])
      [
      Cyclic.Order2,
      Cyclic.Order1,
      Cyclic.Rect1
      ]

justVariantsWithoutRotations :: Size -> [MatrixVariants]
justVariantsWithoutRotations sz =
  let interleave = mkInterleaveVariation sz
      modulation = mkModulateVariation sz
  in map (\y -> y Nothing)
       [ Variants (pure interleave)
       , Variants (pure modulation)
       , Variants (modulation :| [interleave])
       , Variants (pure interleave) . Just . Variants (pure modulation)]

allWorlds :: [SmallWorldCharacteristics]
allWorlds =
  map (\(a,b,c) -> SWCharacteristics a b c) params
 where
  params =
    (Size 32 72, ComponentCount 1, 0.2):
    (Size  8 18, ComponentCount 1, 0.5):
    (Size  8 18, ComponentCount 1, 0.6):
    (Size  8 18, ComponentCount 1, 0.7):
    []

-- | 'SmallWorldCharacteristics's are ordered by difficulty (for a single component, the higher the probability,
-- the more difficult it is to find a valid world.)
exhaustiveWorlds :: [[SmallWorldCharacteristics]]
exhaustiveWorlds =
    map (\sz -> let key = (sz,ComponentCount 1) in worldsFor key) $
    map (\l -> Size l $ fromIntegral l) [8,16,32,64]

 where
  worldsFor (size,componentCount) =
    [SWCharacteristics size componentCount proba |
      proba <- map ((*) 0.1 . fromIntegral) [1 :: Int ..9]]

writeHtmlReport :: Maybe (Time Duration System)
                -> Results
                -> UserIntent
                -> IO ()
writeHtmlReport mayAllowedDt allRes intent = do
  html <- forM (Map.assocs allRes)
    (\(worldCharac, worldResults) -> do
        let labelsAndEitherTimeoutsTimes = map
              (\(strategy, results) ->
                ( fromString $ prettyShowMatrixVariants strategy
                , results)) $ Map.assocs worldResults
            resultsAndCS = showTestResults mayAllowedDt -- map these lines to individual results
              labelsAndEitherTimeoutsTimes
              $ (fromString $ prettyShowSWCharacteristics worldCharac :: ColorString)
        return resultsAndCS) >>=
          renderResultsHtml intentStr
            . concatMap
              (\(res,ls) -> map (\l -> (l, fmap toTitle res)) ls)
              . mconcat
  CS.putStrLn $ CS.colored ("Wrote Chrome-compatible html report: " <> pack html) yellow
 where
  intentStr = case intent of
    Cancel -> "The test was interrupted."
    Report _ -> "The test is still running."
    Run -> "The test has finished." -- because we write a report only at the end, or on 'Report'
    Pause _ -> "The test is paused" -- should not happen


  showAsCs :: TestStatus Statistics -> [ColorString]
  showAsCs NotStarted = [CS.colored "NotStarted" blue]
  showAsCs Timeout = [CS.colored "Timeout" red]
  showAsCs (Finished _ stat) = map fromString $ prettyShowStats stat

  toTitle :: TestDurations Statistics
          -> [[ColorString]]
  toTitle (TD m) =
    map (\(seed, stat) ->
      CS.colored' (pack $ show seed) seedColor:
      showAsCs stat)
      $ Map.assocs m
   where
     seedColor = LayeredColor (gray 5) (gray 17)

forMLoudly :: (Show a) => String -> [a] -> (a -> IO b) -> IO [b]
forMLoudly name l act = do
  let count = length l
  forM (zip [1 :: Int ..] l)
    (\(i,v) -> do
      putStrLn $ unwords [name, show i, "of", show count, ":", show v]
      act v)


foldMInterruptible :: (Show a)
                   => IO Bool
                   -- ^ Should the fold continue?
                   -> String
                   -> b
                   -> [a]
                   -> (b -> a -> IO b)
                   -> IO b
foldMInterruptible canContinue name zero l f =
  foldM (\b (i,a) -> do
    let inform = putStrLn $ unwords [name, show i, "of", show total, ":", show a]
    canContinue >>= \case
      True -> do
        inform
        f b a
      False -> do
        inform
        putStrLn "[Skipped]"
        return b)
    zero
    $ zip [1 :: Int ..] l
 where
  total = length l

data TestException = TestInterruptedByUser
    deriving Show
instance Exception TestException

setToFalseOnTermination :: MVar UserIntent -> IO ()
setToFalseOnTermination intent = do
  mainTid <- myThreadId
  mapM_ (installHandlers mainTid) [sigINT, sigTERM]
 where
  installHandlers :: ThreadId -> CInt -> IO ()
  installHandlers mainTid sig =
    void $ installHandler sig (Catch $ handleTermination mainTid) Nothing

  handleTermination :: ThreadId -> IO ()
  handleTermination mainTid =
    modifyMVar_ intent $ \case
      Cancel -> do
        CS.putStrLn $ CS.colored
          "\nUser forced test termination."
          red
        threadDelay 100000
        void $ throwTo mainTid TestInterruptedByUser
        return Cancel
      _ -> do
        CS.putStrLn $ CS.colored
          "\nUser requested test termination. A report will be generated, unless user requests termination one more time."
          orange
        return Cancel

data UserIntent =
    Run
  | Pause !UserIntent
  | Cancel
  | Report !UserIntent
  deriving(Generic, Show)
instance NFData UserIntent

firstNonReport :: UserIntent -> UserIntent
firstNonReport (Report x) = firstNonReport x
firstNonReport y = y

type Results = Map SmallWorldCharacteristics (Map (Maybe MatrixVariants) (TestDurations Statistics))
--newtype Tests = T (Map SmallWorldCharacteristics WorldTests)
--newtype WorldTests = WT (Map (Maybe MatrixVariants) (TestStatus Statistics))

mkEmptyResults :: Results
mkEmptyResults = Map.empty

addResult :: SmallWorldCharacteristics -> Maybe MatrixVariants -> SeedNumber -> TestStatus Statistics -> Results -> Results
addResult w strategy seed stats res =
  Map.alter
    (Just . maybe
      (Map.singleton strategy s)
      (Map.alter (Just . maybe s (mappend s)) strategy))
    w
    res
 where
  s = TD $ Map.singleton seed stats

-- 70 strategies * 160 tests = 11200
-- seed = 256 * 64 o = 16 ko  -> 183 Mo for all tests and strategies

-- 2 step algorithm:
--
-- 1. for each world, find /a/ strategy that works within a given time upper bound.
--
-- (optional:
--   At the end of the time, save and restore the state of RNG so that the times are cumulative (to not lose time).
--   Interrupt a test cleanly, ie without timeout directly: every 1000 matrix, check for continuation
--   which will check for elapsed time.)
-- If we don't do that, change the seed number at each timeout increment.
--
-- if we found a best for a world, start from that strategy for the next world.
-- we don't need to test all strategies, we just want to find one that works with this timeout.
-- we will refine later on.
--
-- 2. Now that we have for each world an upper bound: find /the/ fastest strategy using that upper bound.
withTestScheduler' :: [[SmallWorldCharacteristics]]
                   -- ^ Within inner-lists, 'SmallWorldCharacteristics' are ordered by increasing estimated
                   -- difficulty (i.e increasing estimated time to find a valid world)
                   -> (Size -> [Maybe MatrixVariants])
                   -> MVar UserIntent
                   -> (Time Duration System -> Properties -> GenIO -> IO (MkSpaceResult a, Statistics))
                   -> IO Results
withTestScheduler' worldsByDifficulty mkStrategies intent testF = do
  informStep dt0 allCombinationsByDifficulty
  go dt0 mempty allCombinationsByDifficulty [] Map.empty
 where
  go :: Time Duration System
     -> IntSet
     -- ^ Hints for the key of the fastest strategies
     -> [[(SmallWorldCharacteristics, IntMap (Maybe MatrixVariants))]]
     -- ^ We test the first element of each inner list (the elements thereafter are
     -- considered too difficult and will be tested at a later step).
     -> [[(SmallWorldCharacteristics, IntMap (Maybe MatrixVariants))]]
     -> Results
     -> IO Results
  go !dt hints remaining noValidStrategy oneValidStrategy = continue intent >>= bool
    (return oneValidStrategy)
    (case remaining of
      [] ->
        if null noValidStrategy
          then
            return oneValidStrategy
          else do
            let newDt = nextDt dt
            informStep newDt noValidStrategy
            go newDt hints noValidStrategy [] oneValidStrategy
      []:rest -> go dt hints rest noValidStrategy oneValidStrategy
      easyAndDifficult:nextWorlds -> do
        (remainingEasyDifficult, newHints, newNoValidStrategy, newOneValidStrategy) <-
          tryWorlds easyAndDifficult hints noValidStrategy oneValidStrategy
        go dt newHints (remainingEasyDifficult:nextWorlds) newNoValidStrategy newOneValidStrategy
       where
        tryWorlds [] hs noValid oneValid = return ([], hs, noValid, oneValid)
        tryWorlds l@((easy,strategies):difficults) hs noValid oneValid = do
          let orderedStrategies =
                IMap.assocs (IMap.restrictKeys strategies hs) ++
                IMap.assocs (IMap.withoutKeys strategies hs)
          go' easy orderedStrategies >>= maybe
            (return ([], hs, l:noValid, oneValid))
            (\((newHint,strategy),res) -> case res of
                Timeout -> error "logic"
                NotStarted -> error "logic"
                Finished{} -> do
                  informSuccess easy strategy res
                  informHint hs newHint strategy
                  let newOneValid = addResult easy strategy seedN res oneValid
                  writeHtmlReport (Just dt) newOneValid (Report Run)
                  -- we also try harder worlds now:
                  tryWorlds difficults (ISet.insert newHint hs) noValid newOneValid)
         where
          go' _ [] = return Nothing
          go' world (assoc@(_,strategy):otherStrategies) =
            continue intent >>= bool
              (return Nothing)
              (do
                  putStrLn $ "try " ++ show strategy
                  initialize (fromList $ deterministicMWCSeeds !! fromIntegral seedN) >>=
                    testF dt (mkProperties world strategy) >>= \(res,stats) -> case mkResultFromStats res stats of
                      Timeout -> go' world otherStrategies
                      f@Finished{} -> return (Just (assoc,f))
                      NotStarted -> error "logic"))

  -- We index the strategies so as to be able to start testing world n+1 with the winning strategy of world n.
  allCombinationsByDifficulty :: [[(SmallWorldCharacteristics, IntMap (Maybe MatrixVariants))]]
  allCombinationsByDifficulty =
    map (map (\w -> (w,IMap.fromList $ zip [0..] $ mkStrategies $ worldSize w))) worldsByDifficulty

  informStep theDt theWorlds =
    mapM_ CS.putStrLn $ showArrayN Nothing $ map (map (W.colorize (onBlack yellow) . fromString))
      [ ["Timeout ", showTime theDt]
      , ["Easy worlds", show nEasy]
      , ["Difficult worlds", show nDifficult]
      ]
   where
     n = sum $ map length theWorlds
     nEasy = length theWorlds
     nDifficult = n - nEasy

  informHint :: IntSet -> Int -> Maybe MatrixVariants -> IO ()
  informHint prev cur strat = bool
    (CS.putStrLn $ CS.colored ("Add hint: " <> pack (show strat)) green)
    (return ())
    $ cur `ISet.member` prev

  informSuccess :: SmallWorldCharacteristics -> Maybe MatrixVariants -> TestStatus Statistics -> IO ()
  informSuccess w v s = do
    print w
    print v
    print s

  nextDt = (.*) multDt
  multDt = 10
  dt0 = fromSecs 0.00001
  seedN = SeedNumber 0

-- note that even in case of timeout we could provide some stats.
mkResultFromStats :: MkSpaceResult a -> Statistics -> TestStatus Statistics
mkResultFromStats res stats = case res of
  NeedMoreTime -> Timeout
  Impossible err -> error $ "impossible :" ++ show err
  Success _ -> Finished (totalDuration $ durations stats) stats


withTestScheduler :: [SmallWorldCharacteristics]
                  -> (Size -> [Maybe MatrixVariants])
                  -> Time Duration System
                  -> MVar UserIntent
                  -> (Properties -> GenIO -> IO Statistics)
                  -> IO Results
withTestScheduler worlds strategies allowed intent f =
  foldMInterruptible cont "Seed" mkEmptyResults [1..nSeeds] (\res0 seed@(SeedNumber i) ->
    foldMInterruptible cont "World" res0 worlds (\res1 world -> do
      let strats = strategies $ worldSize world
      foldMInterruptible cont "Strategy" res1 strats (\res2 strategy -> do
        report res2
        -- For easier reproductibility, eventhough the choice of seed is on the outer loop,
        -- we initialize the generator here.
        gen <- initialize $ fromList $ deterministicMWCSeeds !! i
        let test = f (mkProperties world strategy) gen
        flip (addResult world strategy seed) res2 <$> withTimeout allowed test)))
 where
  cont = continue intent
  report = mayReport allowed intent

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

mayReport :: Time Duration System -> MVar UserIntent -> Results -> IO ()
mayReport allowed intent x = readMVar intent >>= \case
  i@(Report prevIntent) -> do
    writeHtmlReport (Just allowed) x i
    modifyMVar_ intent $ const $ return $ firstNonReport prevIntent
    return ()
  _ -> return ()

mkTerminator :: IO (MVar UserIntent)
mkTerminator = do
  b <- newMVar Run
  void $ forkIO $ do
    hSetBuffering stdin NoBuffering

    let skipRepeats = void $ timeout 1000000 $ forever $ do
          void getChar
          CS.putStrLn $ CS.colored "\nA key-press was ignored. Please try again in a second." yellow

    forever $ getChar >>= \case
      'r' -> do
        CS.putStrLn $ CS.colored "\nAn intermediate html report will be generated as soon as possible..." yellow
        modifyMVar_ b $ \prevIntent -> return $ Report prevIntent
        skipRepeats
      ' ' -> do
        modifyMVar_ b $ \case
          Pause x -> do
            CS.putStrLn $ CS.colored ("\nTest state is " <> pack (show x)) yellow
            return x
          s -> do
            CS.putStrLn $ CS.colored "\nTest will pause as soon as possible, please wait..." yellow
            return $ Pause s
        skipRepeats
      _ -> return ()
  setToFalseOnTermination b
  return b

profileAllProps2 :: IO ()
profileAllProps2 = do
  putStrLn " - Worlds:"
  mapM_ (putStrLn . prettyShowSWCharacteristics) $ List.concat worlds
  intent <- mkTerminator

  (totalDt, allRes) <- withDuration $
    withTestScheduler' worlds strategies intent (\dt property seed -> do
      c <- newMVar True
      _ <- forkIO $ do
        threadDelay $ fromIntegral $ toMicros dt
        modifyMVar_ c $ const $ return False -- TODO measure overhead vs. using timeout
      profileWithContinue property seed c)

  readMVar intent >>= writeHtmlReport Nothing allRes
  putStrLn $ "Test duration = " ++ show totalDt
 where

  worlds = exhaustiveWorlds --allWorlds
  strategies size =
    map
      Just
      (concatMap
        (justVariantsWithRotations size)
        margins ++
      justVariantsWithoutRotations size) ++
    [Nothing] -- i.e use only random matrices.

  -- NOTE we don't use margin 0 because for single component worlds, it is strictly equivalent to never rotating.
  -- For multiple component worlds however, it is not equivalent, since after the nComponents test,
  -- maybe the spacewellused or well distributed test could fail.
  margins = [1..7] -- TODO test higher margins

profileAllProps :: IO ()
profileAllProps = do
  putStrLn " - Worlds:"
  mapM_ (putStrLn . prettyShowSWCharacteristics) worlds
  intent <- mkTerminator

  (totalDt, allRes) <- withDuration $
    withTestScheduler worlds strategies allowedDt intent (\property seed ->
      snd <$> profile property seed)

  readMVar intent >>= writeHtmlReport (Just allowedDt) allRes
  putStrLn $ "Test duration = " ++ show totalDt
 where

  !allowedDt = fromSecs 15

  worlds = allWorlds
  strategies size =
    map
      Just
      (concatMap
        (justVariantsWithRotations size)
        margins ++
      justVariantsWithoutRotations size) ++
    [Nothing] -- i.e use only random matrices.

  -- NOTE we don't use margin 0 because for single component worlds, it is strictly equivalent to never rotating.
  -- For multiple component worlds however, it is not equivalent, since after the nComponents test,
  -- maybe the spacewellused or well distributed test could fail.
  margins = [1..7] -- TODO test higher margins


profile :: Properties -> GenIO -> IO (MkSpaceResult SmallWorld, Statistics)
profile p gen = newMVar True >>= profileWithContinue p gen

profileWithContinue :: Properties -> GenIO -> MVar Bool -> IO (MkSpaceResult SmallWorld, Statistics)
profileWithContinue property gen c = do
  (res, stats) <- mkSmallWorld gen property $ readMVar c
  case res of
    NeedMoreTime -> return ()
    Impossible err -> error $ "impossible :" ++ show err
    Success _ -> return ()
  return (res, stats)

profileLargeWorld :: IO ()
profileLargeWorld = do
  let props = mkProperties
        (SWCharacteristics (Size 8 18) (ComponentCount 1) 0.7)
        (Just $ Variants (pure $ Rotate $ RotationDetail Cyclic.Order2 5) Nothing)
  print props
  withNumberedSeed (withDuration . profile props) (SeedNumber 2) >>= print

profileInterleave0MarginRotateOrder1 :: IO ()
profileInterleave0MarginRotateOrder1 = do
  let sz = Size 32 72
      props = mkProperties
        (SWCharacteristics sz (ComponentCount 1) 0.2)
        (Just $ Variants (pure $ mkInterleaveVariation sz) $ Just $ Variants (pure $ Rotate $ RotationDetail Cyclic.Order1 0) Nothing)
  print props
  withNumberedSeed (withDuration . profile props) (SeedNumber 0) >>= print

nSeeds :: SeedNumber
nSeeds = 10

-- | Runs the action several times, with different - deterministically seeded - generators.
withDifferentSeeds :: (SeedNumber -> GenIO -> IO a) -> IO (Map SeedNumber a)
withDifferentSeeds act =
  Map.fromAscList <$> mapM (\n -> (,) n <$> withNumberedSeed (act n) n) [0..nSeeds - 1]


withNumberedSeed :: (GenIO -> IO a) -> SeedNumber -> IO a
withNumberedSeed act (SeedNumber i) = do
  -- we use deterministic seeds made from the same source as 'withSystemRandom' (see https://github.com/bos/mwc-random/issues/64)
  gen <- initialize $ fromList $ deterministicMWCSeeds !! i
  putStrLn $ unwords ["-", "seed", show i]
  act gen

withTimeout :: Time Duration System -> IO a -> IO (TestStatus a)
withTimeout dtSecs = fmap mkStatus . timeout dt . withDuration
 where
  dt = fromIntegral $ toMicros dtSecs

writeSeedsSource :: IO ()
writeSeedsSource =
  mkSeedsSourceFile 100 >>= writeFile "./imj-profile/src/Imj/Random/MWC/Seeds.hs"

mkSeedSystem :: IO [Word32]
mkSeedSystem =
  -- drop initial index and carry which are not needed
  reverse . drop 2 . reverse . U.toList . fromSeed <$> (save =<< createSystemRandom)

mkSeedsSourceFile :: Int -> IO String
mkSeedsSourceFile n = do
  allSeeds <- (\x -> [" [", unlines $ intersperse " ," x, " ]"]) <$>
    replicateM n (do
      allNums <- unlines . map ((++) "    " . List.concat) . splitEvery 8 . words . List.intercalate ", " . map show <$> mkSeedSystem
      return $ unlines ["  [", allNums, "  ]"])
  return $ unlines $
    [ "-- | The source code for this module was generated by 'writeSeedsSource'"
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
