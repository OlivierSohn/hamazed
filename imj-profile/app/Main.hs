{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import           Imj.Prelude

import           Prelude(print, putStrLn, length, writeFile)
import           Control.Concurrent.MVar.Strict (MVar, newMVar, modifyMVar_, readMVar)
import           Data.List.NonEmpty (NonEmpty(..))
import           Data.List as List hiding(intercalate, concat)
import qualified Data.List as List(intercalate, concat)
import           Data.String(IsString(..))
import           Data.Vector(fromList)
import qualified Data.Vector.Unboxed as U(toList)
import           Data.Word(Word32)
import           Foreign.C.Types(CInt)
import           System.Posix.Signals (installHandler, Handler(..), sigINT, sigTERM)
import           System.Random.MWC
import           System.Timeout(timeout)

import qualified Imj.Data.Matrix.Cyclic as Cyclic

import qualified Imj.Graphics.Text.ColorString as CS(putStrLn)
import           Imj.Game.Hamazed.World.Space.Types
import           Imj.Game.Hamazed.World.Space
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

forMLoudly :: (Show a) => String -> [a] -> (a -> IO b) -> IO [b]
forMLoudly name l act = do
  let count = length l
  forM (zip [1 :: Int ..] l)
    (\(i,v) -> do
      putStrLn $ unwords [name, show i, "of", show count, ":", show v]
      act v)

setToFalseOnTermination :: MVar Bool -> IO ()
setToFalseOnTermination continue = mapM_ installHandlers
   [sigINT, sigTERM]
 where
  installHandlers :: CInt -> IO ()
  installHandlers sig =
    void $ installHandler sig (Catch handleTermination) Nothing

  handleTermination :: IO ()
  handleTermination =
    modifyMVar_ continue $ const $ return False

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
  continue <- newMVar True
  setToFalseOnTermination continue
  -- run benchmarks
  (TestDuration totalDt allRes) <- withTestDuration
    (zip worlds <$> forMLoudly "World" worlds
      (\worldCharacteristics -> readMVar continue >>= \case
         True ->
          zip strategies <$> forMLoudly "Strategy" strategies
            (\strategy -> readMVar continue >>= \case
               True -> do
                let p = mkProperties worldCharacteristics strategy
                res <- timeWithDifferentSeeds (\seed ->
                  readMVar continue >>= bool
                    (putStrLn "skipped" >> return Nothing)
                    -- fmap void snd : to drop the world results so that memory is not wasted.
                    (timeout allowedDtMicros $ fmap (void snd) $ profile p seed))
                bool Nothing (Just res) <$> readMVar continue -- avoid truncated measurements
               False -> do
                 putStrLn "skipped"
                 return Nothing)
         False -> do
           putStrLn "skipped"
           return []))
  -- print results
  putStrLn $ "Timeout = " ++ show allowedDt
  let printInterrupted = readMVar continue >>= bool
        (putStrLn "Test was interrupted.")
        (return ())
      withInterrupted x = do
        printInterrupted
        void x
        printInterrupted

  withInterrupted $ do
    forM_ allRes
      (\(worldCharac, worldResults) -> do
        let labelsAndEitherTimeoutsTimes = map
              (\(strategy, mayResults) ->
                ( fromString $ prettyShowMatrixVariants strategy
                , maybe
                    Cancelled
                    (\results ->
                      let tds = testDurations results
                      in case length tds - length (mapMaybe testResult tds) of
                        0 -> Finished results
                        n -> SomeTimeout n)
                    mayResults
                  ))
              worldResults

        mapM_ CS.putStrLn $
          showTestResults allowedDt
            (map snd labelsAndEitherTimeoutsTimes)
            (map fst labelsAndEitherTimeoutsTimes)
            $ fromString $ prettyShowSWCharacteristics worldCharac)
    putStrLn $ "Actual test duration = " ++ show totalDt
 where
  -- time allowed for each individual seed
  !allowedDt = fromSecs 0.04 --40 -- TODO this timeout should be dynamic
  -- in general, it would be interesting to chose the seed on the outer loop.
  !allowedDtMicros = fromIntegral $ toMicros allowedDt

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
  withNumberedSeed (withTestDuration . profile props) 0 >>= print

profileInterleave0MarginRotateOrder1 :: IO ()
profileInterleave0MarginRotateOrder1 = do
  let props = mkProperties
        (SWCharacteristics (Size 32 72) (ComponentCount 1) 0.2)
        (Just $ Variants (pure Interleave) $ Just $ Variants (pure $ Rotate $ RotationDetail 0 Cyclic.Order1) Nothing)
  print props
  withNumberedSeed (withTestDuration . profile props) 0 >>= print
--  withDifferentSeeds (withTestDuration . profile props) >>= print

nSeeds :: Int
nSeeds = 11

-- | Runs the action several times, with different - deterministically seeded - generators.
withDifferentSeeds :: (GenIO -> IO a) -> IO [a]
withDifferentSeeds act =
  mapM (withNumberedSeed act) [0..nSeeds - 1]

withDifferentSeeds_ :: (GenIO -> IO a) -> IO ()
withDifferentSeeds_ = void . withDifferentSeeds

withNumberedSeed :: (GenIO -> IO a) -> Int -> IO a
withNumberedSeed act i = do
  -- we use deterministic seeds made from the same source as 'withSystemRandom' (see https://github.com/bos/mwc-random/issues/64)
  gen <- initialize $ fromList $ deterministicMWCSeeds !! i
  putStrLn $ unwords ["-", "seed", show (i :: Int)]
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
