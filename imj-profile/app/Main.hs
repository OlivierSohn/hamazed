{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import           Imj.Prelude

import           Prelude(print, putStrLn, length)
import           Control.Concurrent(getNumCapabilities)
import           Control.Concurrent.MVar.Strict (MVar, newMVar, readMVar)
import           Data.Either(isRight)
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import           Data.List
import           Data.Set(Set)
import qualified Data.Set as Set
import           Data.UUID(UUID)
import           Data.Vector(fromList)
import           System.Random.MWC
import           System.Timeout(timeout)

import           Imj.Data.AlmostFloat
import           Imj.Game.Hamazed.World.Space.Types
import           Imj.Game.Hamazed.World.Types

import           Imj.Control.Concurrent
import qualified Imj.Data.Matrix.Cyclic as Cyclic
import           Imj.Game.Hamazed.World.Size
import           Imj.Game.Hamazed.World.Space
import           Imj.Graphics.Text.Render
import           Imj.Profile.Intent
import           Imj.Profile.Render
import           Imj.Profile.Result
import           Imj.Profile.Results
import           Imj.Profile.Scheduler
import           Imj.Random.Image
import           Imj.Random.MWC.Image
import           Imj.Random.MWC.Seeds
import           Imj.Random.MWC.Util
import           Imj.Random.Test
import           Imj.Random.Util
import           Imj.Timing

-- commands used to profile:
--
-- for cost centers:
-- stack build --executable-profiling --library-profiling --ghc-options="-fprof-auto -rtsopts" && stack exec -- imj-profile-exe +RTS -M2G -p
--
-- for ticky-ticky : add ticky option at compile time (in the module, or in stack.yaml) +
-- stack build && stack exec -- imj-profile-exe +RTS -rprofile.ticky

main :: IO ()
main = do
  useOneCapabilityPerPhysicalCore
  --profileLargeWorld -- simple benchmark, used as ref for benchmarking a new algo
  --profileAllProps -- exhaustive benchmark, to study how to tune strategy wrt world parameters
  mkOptimalStrategies -- computes one optimal strategy per possible world in hamazed, and serializes the result.
  --writeRNGImages
  --measureRNGsSpeed
  --writeSeedsSource

measureRNGsSpeed :: IO ()
measureRNGsSpeed = do
  r1 <- withMWC256 test
  r2 <- withRandom test
  r2L <- withRandomL testL

  mapM_ putStrLn $ showArrayN Nothing $ map
    (\(name,(dt,n)) ->[name, show n, showTime dt])
    [r1, r2, r2L]


  -- random
 where
  test gen =
    withDuration $ splitRandWord32 10000000 (quot maxBound 2) gen
  testL gen =
    withDuration $ splitRandWord32L 10000000 (quot maxBound 2) gen

writeRNGImages :: IO ()
writeRNGImages =
  forM_ [Size 768 1024] $ \sz -> do
    forM_ colorGens $ \(g,name) ->
      g seed sz >>= writeRndImage seed name sz Nothing
    forM_ grayGens8 $ \(g,name) ->
      g seed sz >>= writeRndImage seed name sz Nothing
    forM_ grayGens32 $ \(g,name) ->
      g seed sz >>= writeRndImage seed name sz Nothing
    forM_ bwGens $ \(g,name) ->
      forM_ probas $ \proba ->
        g seed sz proba >>= writeRndImage seed name sz (Just proba)
 where
  seed = SeedNumber 0

  bwGens = [(mkMWC256Image, "mwc256")
          , (mkSystemRandomImage, "random")]
  grayGens8 = [(mkMWC256ImageGray, "mwc256")
              ]
  grayGens32 = [
             (mkMWC256ImageGray', "mwc256gray")
             ]
  colorGens = [(mkMWC256ImageRGB, "mwc256color")]

  probas = [0.1, 0.01, 0.5, 0.9, 0.99]

justVariantsWithRotations :: ComponentCount -> [MatrixVariantsSpec] -- 24
justVariantsWithRotations n =
  let interleave = Interleaving
      modulation = Modulation
  in concatMap (map (\y -> y Nothing)) $
      map (\rotationOrder ->
        let rotation = Rotation $ RotationDetail rotationOrder n
        in  VariantsSpec (pure interleave) . Just . VariantsSpec (pure modulation) . Just . VariantsSpec (pure rotation):
            VariantsSpec (modulation :| [interleave]) . Just . VariantsSpec (pure rotation):
            VariantsSpec (pure interleave) . Just . VariantsSpec (pure rotation):
            VariantsSpec (pure modulation) . Just . VariantsSpec (pure rotation):
            VariantsSpec (interleave :| [rotation]):
            VariantsSpec (modulation :| [rotation]):
            VariantsSpec (interleave :| [modulation,rotation]):
            VariantsSpec (pure rotation):
            [])
      [
      Cyclic.Order2,
      Cyclic.Order1,
      Cyclic.Rect1
      ]

justVariantsWithoutRotations :: [MatrixVariantsSpec] -- 4
justVariantsWithoutRotations =
  let interleave = Interleaving
      modulation = Modulation
  in map (\y -> y Nothing)
       [ VariantsSpec (pure interleave)
       , VariantsSpec (pure modulation)
       , VariantsSpec (modulation :| [interleave])
       , VariantsSpec (pure interleave) . Just . VariantsSpec (pure modulation)]

someWorlds :: Set (SmallWorldCharacteristics Program)
someWorlds =
  Set.fromList $ map (\(a,b,c) -> SWCharacteristics a b c) params
 where
  params =
    (Size 32 72, ComponentCount 1, 0.2):
    (Size  8 18, ComponentCount 1, 0.5):
    (Size  8 18, ComponentCount 1, 0.6):
    (Size  8 18, ComponentCount 1, 0.7):
    []

data TestAssumption =
    DifficultyIncreasesWithProbability
  | MinimalDifficultyAt !AlmostFloat

-- | Based on observed behaviour for 1,2,3 and 4 components.
-- You may need to add values if you need a bigger number of components,
-- and improve the efficiency of optimal strategies precomputation.
mkAssumption :: ComponentCount -> TestAssumption
mkAssumption (ComponentCount n)
  | n <= 1 = DifficultyIncreasesWithProbability
  | n == 2 = MinimalDifficultyAt $ almost 0.49
  | otherwise = MinimalDifficultyAt $ almost 0.39

-- | Inner lists are sorted by estimated difficulty.
-- The outer list, when filtered on a single 'ComponentCount', is sorted by increasing areas of the inner list 'SmallWorldCharacteristics' sizes
exhaustiveWorlds :: [Size]
                 -- ^ The worlds sizes
                 -> [AlmostFloat]
                 -- ^ The wall probabilities
                 -> [[SmallWorldCharacteristics Program]]
exhaustiveWorlds smallSizes probas =
  concatMap
    (\cc ->
      concatMap
        (\sz ->
          map
            (filter isPossible . map (SWCharacteristics sz cc)) $
            orderProbas $ mkAssumption cc)
        $ sortOn area $ smallSizes)
    [1 :: ComponentCount ..4]

 where
  orderProbas = \case
    DifficultyIncreasesWithProbability -> [probas]
    MinimalDifficultyAt t -> [reverse lows,highs]
      where (lows,highs) = partition (< t) probas

  isPossible ch@(SWCharacteristics _ nComponents _) =
    -- we remove impossible worlds : in the game when an impossible configuration
    -- is encountered, we reduce the count of components. Hence
    -- we make sure removed worlds have more than one component.
        isRight (mkLowerBounds ch) || ((nComponents == 1) && error "logic")

withTestScheduler :: UUID
                   -- ^ Test unique identifier
                  -> Set (SmallWorldCharacteristics Program)
                  -> Set (Maybe MatrixVariantsSpec)
                  -> Time Duration System
                  -> MVar UserIntent
                  -> (Properties -> GenIO -> IO Statistics)
                  -> IO (OldMaybeResults SeedNumber)
withTestScheduler key worlds strategies allowed intent testF =
  foldMInterruptible "Seed" (mkNothingResults' worlds) [1..nSeeds] (\res0 seed@(SeedNumber i) ->
    foldMInterruptible "World" res0 (Set.toList worlds) (\res1 world@(SWCharacteristics sz _ _) ->
      foldMInterruptible "Strategy" res1 (Set.toList strategies) (\res2 strategy -> do
        onReport (writeHtmlReport key (resultsToHtml' (Just allowed) res2)) intent
        -- For easier reproductibility, eventhough the choice of seed is on the outer loop,
        -- we initialize the generator here.
        gen <- initialize $ fromList $ deterministicMWCSeeds !! i
        let s = fmap (toVariants sz) strategy
        flip (addResult' world s seed) res2 <$>
          withTimeout (testF (mkProperties world s) gen))))
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

  withTimeout :: (NFData a) => IO a -> IO (TestStatus a)
  withTimeout = fmap mkStatus . timeout dt . withDuration
   where
    dt = fromIntegral $ toMicros allowed


mkOptimalStrategies :: IO ()
mkOptimalStrategies = do
  intent <- mkTerminator

  let testF mayDt property seedGroup = do
        let f = profile property seedGroup
        maybe (fmap Just) (timeout . fromIntegral . toMicros) mayDt f

  progress <- decodeProgressFile >>= maybe (mkZeroProgress (exhaustiveWorlds exhaustiveSmallSizes allProbasForGame) allStrategies) return
  (totalDt, _) <- withDuration $ withTestScheduler' intent testF progress

  putStrLn $ "Test duration = " ++ show totalDt

profileAllProps :: IO ()
profileAllProps = do
  intent <- mkTerminator
  key <- randUUID

  (totalDt, allRes) <- withDuration $
    withTestScheduler key someWorlds allStrategies allowedDt intent (\property seed ->
      snd <$> profile property (pure seed))

  readMVar intent >>= writeHtmlReport key (resultsToHtml' (Just allowedDt) allRes)
  putStrLn $ "Test duration = " ++ show totalDt
 where
  allowedDt = fromSecs 15

allStrategies :: Set (Maybe MatrixVariantsSpec) -- 1 + 4 + 24*7 == 173
allStrategies =
  Set.fromList $ map
    Just
    (concatMap
      justVariantsWithRotations
      margins ++
    justVariantsWithoutRotations) ++
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
  let sz = Size 12 24
      -- (Size 8 18)
      props = mkProperties
        (SWCharacteristics sz (ComponentCount 4) 0.5)
        --(SWCharacteristics sz (ComponentCount 1) 0.7)
        (Just $ (toVariants sz) $ VariantsSpec (pure $ Rotation $ RotationDetail Cyclic.Order2 5) Nothing)
        --(Just $ toVariants sz $ VariantsSpec (pure Interleaving) Nothing)
  print props
  nWorkers <- max 1 <$> getNumCapabilities
  putStrLn $ unwords ["using", show nWorkers, "workers"]
  withNumberedSeeds (withDuration . profile props) (NE.map SeedNumber $ 0:|take (nWorkers-1) [1..]) >>= print
