{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}

module Main where

import           Imj.Prelude

import           Prelude(print, putStrLn, length)
import           Control.Concurrent(getNumCapabilities)
import           Control.Concurrent.MVar.Strict (MVar, newMVar, readMVar)
import           Data.Either(isRight)
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import           Data.List hiding(concat)
import           Data.IntMap.Strict(IntMap)
import qualified Data.IntMap.Strict as IMap
import           Data.UUID(UUID)
import           Data.Vector(fromList)
import           System.Random.MWC
import           System.Timeout(timeout)

import           Imj.Game.Hamazed.Types
import           Imj.Game.Hamazed.World.Space.Types

import           Imj.Control.Concurrent
import qualified Imj.Data.Matrix.Cyclic as Cyclic
import           Imj.Game.Hamazed.World.Size
import           Imj.Game.Hamazed.World.Space
import           Imj.Profile.Intent
import           Imj.Profile.Render
import           Imj.Profile.Result
import           Imj.Profile.Results
import           Imj.Profile.Scheduler
import           Imj.Random.MWC.Seeds
import           Imj.Random.MWC.Util
import           Imj.Random.Util
import           Imj.Timing
import           Imj.Util

-- commands used to profile:
--
-- for cost centers:
-- stack build --executable-profiling --library-profiling --ghc-options="-fprof-auto -rtsopts" && stack exec -- imj-profile +RTS -M2G -p
--
-- for ticky-ticky : add ticky option at compile time (in the module, or in stack.yaml) +
-- stack build && stack exec -- imj-profile-exe +RTS -rprofile.ticky

main :: IO ()
main = do
  useOneCapabilityPerPhysicalCore
  --profileLargeWorld -- simple benchmark, used as ref for benchmarking a new algo
  --profileAllProps -- exhaustive benchmark, to study how to tune strategy wrt world parameters
  profileAllProps2 -- exhaustive benchmark, with notion of easy / hard test to reach approximated results as fast as possible.
  --writeSeedsSource

justVariantsWithRotations :: Size -> ComponentCount -> [MatrixVariants]
justVariantsWithRotations sz n =
  let interleave = mkVariation sz Interleaving
      modulation = mkVariation sz Modulation
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
  let interleave = mkVariation sz Interleaving
      modulation = mkVariation sz Modulation
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
exhaustiveWorldsByDifficulty :: [[SmallWorldCharacteristics]]
exhaustiveWorldsByDifficulty =
  concatMap
    (flip map exhaustiveSmallSizes . flip worldsByIncreasingDifficulty)
    [1 :: ComponentCount ..4]

 where

  possibleWorld ch@(SWCharacteristics _ nComponents _) =
    -- we remove impossible worlds : in the game when an impossible configuration
    -- is encountered, we reduce the count of components. Hence
    -- we make sure removed worlds have more than one component.
        isRight (mkLowerBounds ch) || ((nComponents == 1) && error "logic")

  worldsByIncreasingDifficulty size componentCount =
    filter possibleWorld $
    map (SWCharacteristics size componentCount) allProbasForGame

  exhaustiveSmallSizes =
    sortOn area $ dedup $ map canonicalize $
      concatMap (\s -> map (bigToSmall s) allBlockSizes) bigSizes
   where
    bigSizes = concatMap
      (\l -> map (worldSizeFromLevel l) [Square, Rectangle'2x1])
      [firstLevel..lastLevel]

    canonicalize sz@(Size (Length a) (Length b))
      | a <= b = sz
      | otherwise = Size (fromIntegral b) (fromIntegral a)


withTestScheduler :: UUID
                   -- ^ Test unique identifier
                  -> [SmallWorldCharacteristics]
                  -> (Size -> [Maybe MatrixVariants])
                  -> Time Duration System
                  -> MVar UserIntent
                  -> (Properties -> GenIO -> IO Statistics)
                  -> IO (Results SeedNumber)
withTestScheduler key worlds strategies allowed intent testF =
  foldMInterruptible "Seed" mkEmptyResults [1..nSeeds] (\res0 seed@(SeedNumber i) ->
    foldMInterruptible "World" res0 worlds (\res1 world@(SWCharacteristics sz _ _) -> do
      let strats = strategies sz
      foldMInterruptible "Strategy" res1 strats (\res2 strategy -> do
        onReport (writeHtmlReport key (resultsToHtml (Just allowed) res2)) intent
        -- For easier reproductibility, eventhough the choice of seed is on the outer loop,
        -- we initialize the generator here.
        gen <- initialize $ fromList $ deterministicMWCSeeds !! i
        flip (addResult world strategy seed) res2 <$> withTimeout (testF (mkProperties world strategy) gen))))
 where
  nSeeds = 10

  foldMInterruptible name zero l f =
    foldM (\b (i,a) -> do
      let inform = putStrLn $ unwords [name, show i, "of", show total, ":", show a]
      continue intent >>= \case
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

  withTimeout :: IO a -> IO (TestStatus a)
  withTimeout = fmap mkStatus . timeout dt . withDuration
   where
    dt = fromIntegral $ toMicros allowed


profileAllProps2 :: IO ()
profileAllProps2 = do
  intent <- mkTerminator

  let testF mayDt property seedGroup = do
        let f = profile property seedGroup
        maybe (fmap Just) (timeout . fromIntegral . toMicros) mayDt f

  progress <- decodeProgressFile >>= maybe (mkZeroProgress worlds) return
  (totalDt, _) <- withDuration $ withTestScheduler' intent testF progress

  putStrLn $ "Test duration = " ++ show totalDt
 where
  -- We index the strategies so as to be able to start testing world n+1 with the winning strategy of world n.
  worlds :: [[(SmallWorldCharacteristics, IntMap (Maybe MatrixVariants))]]
  worlds =
    map
      (map (\w@(SWCharacteristics sz _ _) -> (w,IMap.fromList $ zip [0..] $ allStrategies sz)))
      exhaustiveWorldsByDifficulty

profileAllProps :: IO ()
profileAllProps = do
  intent <- mkTerminator
  key <- randUUID

  (totalDt, allRes) <- withDuration $
    withTestScheduler key allWorlds allStrategies allowedDt intent (\property seed ->
      snd <$> profile property (pure seed))

  readMVar intent >>= writeHtmlReport key (resultsToHtml (Just allowedDt) allRes)
  putStrLn $ "Test duration = " ++ show totalDt
 where
  allowedDt = fromSecs 15

allStrategies :: Size -> [Maybe MatrixVariants]
allStrategies size =
  map
    Just
    (concatMap
      (justVariantsWithRotations size)
      margins ++
    justVariantsWithoutRotations size) ++
  [Nothing] -- i.e use only random matrices.
 where
  -- NOTE we don't use margin 0 because for single component worlds, it is strictly equivalent to never rotating.
  -- For multiple component worlds however, it is not equivalent, since after the nComponents test,
  -- maybe the spacewellused or well distributed test could fail.
  margins = [1..7] -- TODO test higher margins


profile :: Properties -> NonEmpty GenIO -> IO (MkSpaceResult SmallWorld, Statistics)
profile p gen = newMVar True >>= profileWithContinue p gen . readMVar

profileWithContinue :: Properties -> NonEmpty GenIO -> IO Bool -> IO (MkSpaceResult SmallWorld, Statistics)
profileWithContinue property gen c = do
  (res, stats) <- mkSmallWorld gen property c
  case res of
    NeedMoreTime -> return ()
    Impossible err -> error $ "impossible :" ++ show err
    Success _ -> return ()
  return (res, stats)

profileLargeWorld :: IO ()
profileLargeWorld = do
  let props = mkProperties
        (SWCharacteristics (Size 8 18) (ComponentCount 1) 0.7)
        --(Just $ Variants (pure $ Rotate $ RotationDetail Cyclic.Order2 5) Nothing)
        (Just $ Variants (pure $ mkVariation (Size 8 18) Interleaving) Nothing)
  print props
  nWorkers <- max 1 <$> getNumCapabilities
  putStrLn $ unwords ["using", show nWorkers, "workers"]
  withNumberedSeeds (withDuration . profile props) (NE.map SeedNumber $ 2:|take (nWorkers-1) [6..]) >>= print
