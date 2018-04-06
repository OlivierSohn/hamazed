{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import           Imj.Prelude

import           Prelude(print, putStrLn, length, writeFile, getChar)
import           Control.Concurrent.MVar.Strict (MVar, newMVar, modifyMVar_, readMVar)
import           Control.Concurrent(forkIO)
import           Control.DeepSeq(NFData(..))
import           Data.List.NonEmpty (NonEmpty(..))
import           Data.List as List hiding(intercalate, concat)
import qualified Data.List as List(intercalate, concat)
import           Data.Map.Strict(Map)
import qualified Data.Map.Strict as Map
import           Data.String(IsString(..))
import           Data.Vector(fromList)
import qualified Data.Vector.Unboxed as U(toList)
import           Data.Word(Word32)
import           Foreign.C.Types(CInt)
import           System.Posix.Signals (installHandler, Handler(..), sigINT, sigTERM)
import           System.Random.MWC
import           System.Timeout(timeout)
import           System.IO(hSetBuffering, stdin, BufferMode(..))

import qualified Imj.Data.Matrix.Cyclic as Cyclic

import qualified Imj.Graphics.Text.ColorString as CS(putStrLn)
import           Imj.Game.Hamazed.World.Space.Types
import           Imj.Game.Hamazed.World.Space
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
  --profileLargeWorld -- simple benchmark, used as ref for benchmarking a new algo
  profileAllProps -- exhaustive benchmark, to study how to tune strategy wrt world parameters
  --writeSeedsSource

justVariantsWithRotations :: ComponentCount -> [MatrixVariants]
justVariantsWithRotations n =
  concatMap
    (\rotationOrder ->
      let rotation =
            Rotate $ RotationDetail n rotationOrder
      in map (\x -> x Nothing)
          [ Variants (pure rotation)
          , Variants (pure Interleave) . Just . Variants (pure rotation)
          , Variants (Interleave :| [rotation])
          ])
    [ Cyclic.Rect1
    , Cyclic.Order1
    , Cyclic.Order2
    ]

justVariantsWithoutRotations :: [MatrixVariants]
justVariantsWithoutRotations = [Variants (pure Interleave) Nothing]

allWorlds :: [SmallWorldCharacteristics]
allWorlds =
  map (\(a,b,c) -> SWCharacteristics a b c) params
 where
  params =
    [ (Size 32 72, ComponentCount 1, 0.2)
    , (Size  8 18, ComponentCount 1, 0.5)
    , (Size  8 18, ComponentCount 1, 0.6)
    , (Size  8 18, ComponentCount 1, 0.7)
    ]

writeHtmlReport :: Time Duration System
                -> Results
                -> UserIntent
                -> IO ()
writeHtmlReport allowedDt allRes intent = do
  putStrLn $ "Timeout = " ++ show allowedDt
  printInterrupted
  write
  printInterrupted

 where
  toTitle (Finished (TD l _)) =
    map
      (\(TestDuration _ res) -> maybe ["error : no stat"] (map fromString . prettyShowStats) res)
      l
  toTitle x = [[fromString $ show x]]

  printInterrupted = case intent of
        Cancel -> putStrLn "Test was interrupted."
        Report -> putStrLn "Test is still running."
        _ -> return ()

  write = do
    html <- forM (Map.assocs allRes)
      (\(worldCharac, worldResults) -> do
          let labelsAndEitherTimeoutsTimes = map
                (\(strategy, results) ->
                  ( fromString $ prettyShowMatrixVariants strategy
                  , let tds = testDurations results
                    in case length tds - length (mapMaybe testResult tds) of
                      0 -> Finished results
                      n -> SomeTimeout n)) $ Map.assocs worldResults
              resultsAndCS = showTestResults allowedDt -- map these lines to individual results
                labelsAndEitherTimeoutsTimes
                $ fromString $ prettyShowSWCharacteristics worldCharac
          mapM_ CS.putStrLn $ mconcat $ map snd resultsAndCS
          return resultsAndCS) >>=
            renderResultsHtml
              . concatMap
                (\(res,ls) -> map (\l -> (l, fmap toTitle res)) ls)
                . mconcat
    putStrLn $ "Wrote html report: " ++ html

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
foldMInterruptible continue name zero l f =
  foldM (\b (i,a) -> do
    putStrLn $ unwords [name, show i, "of", show total, ":", show a]
    continue >>= \case
      True ->
        f b a
      False -> do
        putStrLn "[Skipped]"
        return b)
    zero
    $ zip [0 :: Int ..] l
 where
  total = length l

setToFalseOnTermination :: MVar UserIntent -> IO ()
setToFalseOnTermination continue = mapM_ installHandlers
   [sigINT, sigTERM]
 where
  installHandlers :: CInt -> IO ()
  installHandlers sig =
    void $ installHandler sig (Catch handleTermination) Nothing

  handleTermination :: IO ()
  handleTermination =
    modifyMVar_ continue $ const $ return Cancel

data UserIntent =
    NoIntent
  | Cancel
  | Report
  deriving(Generic)
instance NFData UserIntent

data LoopAction =
    Continue
  | AbortReturnPartialResults
  | AbortReturnFullResults

type Results = Map SmallWorldCharacteristics (Map (Maybe MatrixVariants) (TestDurations (Maybe Statistics)))

mkEmptyResults :: Results
mkEmptyResults = Map.empty

addResult :: SmallWorldCharacteristics -> Maybe MatrixVariants -> TestDurations (Maybe Statistics) -> Results -> Results
addResult w strategy stats res =
  Map.alter
    (Just . maybe
      (Map.singleton strategy stats)
      (Map.alter (Just . maybe stats (error "a result already exits for this strategy")) strategy))
    w
    res

withTestScheduler :: [SmallWorldCharacteristics]
                  -> [Maybe MatrixVariants]
                  -> Time Duration System
                  -> MVar UserIntent
                  -> (Properties -> GenIO -> IO Statistics)
                  -> IO (TestDuration Results)
withTestScheduler worlds strategies allowed intent f =
  withTestDuration $
    foldMInterruptible continue "World" mkEmptyResults worlds (\res world ->
      foldMInterruptible continue "Strategy" res strategies (\res' strategy -> do
        -- Note that we don't report from inside timeWithDifferentSeeds,
        -- to avoid incomplete time series.
        mayReport res'
        let properties = mkProperties world strategy
        flip (addResult world strategy) res' <$>
          timeWithDifferentSeeds (\gen ->
            continue >>= \case
              True -> timeout allowedMicros $ f properties gen
              False -> return Nothing)))
 where
  !allowedMicros = fromIntegral $ toMicros allowed

  continue = (\case
    Cancel -> False
    _ -> True) <$> readMVar intent

  mayReport x = readMVar intent >>= \case
    Report -> do
      writeHtmlReport allowed x Report
      modifyMVar_ intent $ const $ return NoIntent
      return ()
    _ -> return ()

mkTerminator :: IO (MVar UserIntent)
mkTerminator = do
  b <- newMVar NoIntent
  void $ forkIO $ do
    hSetBuffering stdin NoBuffering
    forever $ getChar >>= \case
      'r' -> do
        modifyMVar_ b $ const $ return Report
        void $ timeout 5000000 $ void getChar -- to avoid key repeats
      _ -> return ()
  setToFalseOnTermination b
  return b

profileAllProps :: IO ()
profileAllProps = do
  -- display global test info
  putStrLn " - Worlds:"
  mapM_ (putStrLn . prettyShowSWCharacteristics) worlds
  putStrLn " - Strategies:"
  mapM_ print strategies
  let ntests = length worlds * length strategies * nSeeds
  putStrLn $ "starting " ++ show ntests ++ " tests, with timeout " ++ show allowedDt
  putStrLn $ "Max overall duration = " ++ show (fromIntegral ntests .* allowedDt)
  -- setup handlers to stop the test with Ctrl+C if needed, and still get some results.
  intent <- mkTerminator
  -- run benchmarks
  (TestDuration totalDt allRes) <-
    withTestScheduler worlds strategies allowedDt intent (\property seed ->
      snd <$> profile property seed)

  readMVar intent >>= writeHtmlReport allowedDt allRes
  putStrLn $ "Test duration = " ++ show totalDt
 where

  -- time allowed for each individual seed
  !allowedDt = fromSecs 40 -- TODO this timeout should be dynamic

  worlds = allWorlds
  strategies =
    Nothing : -- i.e no variant, use only random matrices.
    map
      Just
      (justVariantsWithoutRotations ++ -- variants using only interleaved variations
      concatMap
        justVariantsWithRotations -- variants using rotations
        margins)
  margins = [1..7]


profile :: Properties -> GenIO -> IO (MkSpaceResult SmallWorld, Statistics)
profile property gen = do
  (res, stats) <- mkSmallWorld gen property neverInterrupt
  case res of
    NeedMoreTime -> error "test logic" -- since we bever interrupt the test, this is not possible.
    Impossible err -> error $ "impossible :" ++ show err
    Success _ -> return ()
  return (res, stats)
 where
  neverInterrupt = pure True

profileLargeWorld :: IO ()
profileLargeWorld = do
  let props = mkProperties
        (SWCharacteristics (Size 8 18) (ComponentCount 1) 0.7)
        (Just $ Variants (pure $ Rotate $ RotationDetail 5 Cyclic.Order2) Nothing)
  print props
  withNumberedSeed (withTestDuration . profile props) (SeedNumber 0) >>= print

profileInterleave0MarginRotateOrder1 :: IO ()
profileInterleave0MarginRotateOrder1 = do
  let props = mkProperties
        (SWCharacteristics (Size 32 72) (ComponentCount 1) 0.2)
        (Just $ Variants (pure Interleave) $ Just $ Variants (pure $ Rotate $ RotationDetail 0 Cyclic.Order1) Nothing)
  print props
  withNumberedSeed (withTestDuration . profile props) (SeedNumber 0) >>= print
--  withDifferentSeeds (withTestDuration . profile props) >>= print

newtype SeedNumber = SeedNumber Int

nSeeds :: Int
nSeeds = 11

-- | Runs the action several times, with different - deterministically seeded - generators.
withDifferentSeeds :: (GenIO -> IO a) -> IO [a]
withDifferentSeeds act =
  mapM (withNumberedSeed act . SeedNumber) [0..nSeeds - 1]

withDifferentSeeds_ :: (GenIO -> IO a) -> IO ()
withDifferentSeeds_ = void . withDifferentSeeds

withNumberedSeed :: (GenIO -> IO a) -> SeedNumber -> IO a
withNumberedSeed act (SeedNumber i) = do
  -- we use deterministic seeds made from the same source as 'withSystemRandom' (see https://github.com/bos/mwc-random/issues/64)
  gen <- initialize $ fromList $ deterministicMWCSeeds !! i
  putStrLn $ unwords ["-", "seed", show i]
  act gen

-- | Drops the first measurement.
timeWithDifferentSeeds :: (GenIO -> IO a) -> IO (TestDurations a)
timeWithDifferentSeeds act =
  mkTestDurations . drop 1 <$> withDifferentSeeds (withTestDuration . act)

withTestDuration :: IO a -> IO (TestDuration a)
withTestDuration = fmap (uncurry TestDuration) . withDuration

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
