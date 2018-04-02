{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Imj.Prelude

import           Prelude(print, putStrLn, putStr, length, writeFile)
import           Control.Concurrent(threadDelay)
import           Data.List as List(foldl', unlines, intersperse)
import qualified Data.List as List(intercalate, concat)
import           Data.String(IsString(..))
import           Data.Map.Strict(Map)
import qualified Data.Map.Strict as Map
import           Data.Vector(fromList)
import qualified Data.Vector.Unboxed as U(toList)
import           Data.Word(Word32)
import           System.IO(hFlush, stdout)
import           System.Random.MWC
import           System.Timeout(timeout)

import           Imj.Data.Class.Quantifiable
import qualified Imj.Data.Matrix.Cyclic as Cyclic

import qualified Imj.Graphics.Text.ColorString as CS(putStrLn)
import           Imj.Game.Hamazed.World.Space.Types
import           Imj.Game.Hamazed.World.Space
import           Imj.Graphics.Text.Render
import           Imj.Timing
import           Imj.Util

import           Imj.Random.MWC.Seeds

-- command used to profile.
--
-- for cost centers:
-- stack build --executable-profiling --library-profiling --ghc-options="-fprof-auto -rtsopts" && stack exec -- imj-profile +RTS -M2G -p
--
-- for ticky-ticky : add ticky option at compile time (in the module, or in stack.yaml) +
-- stack build && stack exec -- imj-profile-exe +RTS -rprofile.ticky

main :: IO ()
main =
  --profileAllProps
  profileLargeWorld
  --measureMemory
  --writeSeedsSource
  --testRNG

measureMemory :: IO ()
measureMemory = do
  gen <- create -- use a deterministic random numbers source
  forever $ do
    delay "Make random mat"
    unsafeMkSmallMat gen 0.7 (Size 1000 1000) >>= \(SmallMatInfo nAir m) -> do
      delay "print all" -- Here we have 126 M
      let l = Cyclic.produceUsefulInterleavedVariations m
      print (length l, map matrixSum l)
      delay "Produce, getTopology, analyze " -- Still 126 M
      mapM
        (\x -> do
          putStr "." >> flush
          return $ getTopology $ SmallMatInfo nAir x) l >>= analyzeDistribution -- ~ 300 M
 where
  flush = hFlush stdout
  delay nextAction = do
    putStr "\nWaiting ... "
    forM_ [0..9::Int] $ \i -> do
      threadDelay $ 1 * 1000 * 1000
      putStr (show i) >> flush
    putStrLn $ '\n' : nextAction
  matrixSum mat =
    let n = Cyclic.nrows mat
        m = Cyclic.ncols mat
    in List.foldl'
        (\s i ->
          List.foldl'
            (\s' j -> s' + case Cyclic.unsafeGet i j mat of
              MaterialAndKey k -> k)
            s
            [0..pred m])
        (0 :: Int)
        [0..pred n]

analyzeDistribution :: [(Maybe ComponentCount, IO ())]
                    -- ^ IO () is the render function
                    -> IO ()
analyzeDistribution l = do
  let d = asDistribution $ mapMaybe fst l
  maybe
    (return ())
    (\(min_,_) -> when (min_ <= 1) $ do
      mapM_ snd l
      mapM_ putStrLn (showDistribution d)
      putStrLn "")
    $ Map.lookupMin d


getTopology :: SmallMatInfo -> (Maybe ComponentCount, IO ())
getTopology r =
  let res = matchTopology NCompsNotRequired (ComponentCount 1) r
      compCount = getComponentCount res
      render = do
        print compCount
        putStrLn ""
        mapM_ putStrLn $ showInBox $ writeWorld r
        putStrLn ""
  in (compCount, render)

-- | Returns 12 combinations
allStrategies :: [SmallWorldCreationStrategy]
allStrategies =
  concatMap
    (\branch ->
      map
        (SWCreationStrategy branch)
        [ Cyclic.Order0
        , Cyclic.AtDistance1
        , Cyclic.Order1
        , Cyclic.Order2
        ])
    [ Rotate
    , InterleavePlusRotate
    , InterleaveTimesRotate
    ]

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

forMLoudly :: String -> [a] -> (a -> IO b) -> IO [b]
forMLoudly name l act = do
  let count = length l
  forM (zip [1 :: Int ..] l)
    (\(i,v) -> do
      putStrLn $ name ++ " " ++ show i ++ " of " ++ show count
      act v)

profileAllProps :: IO ()
profileAllProps = do
  -- run benchmarks
  allRes <- zip allWorlds <$> forMLoudly "World" allWorlds
    (\worldCharacteristics ->
      zip allStrategies <$> forMLoudly "Strategy" allStrategies
        (\strategy -> do
          let p = mkProperties worldCharacteristics strategy
          timeWithDifferentSeeds (timeout allowedMicros . profile p))
      )
  -- print results
  putStrLn $ "Timeout = " ++ show allowed
  forM_ allRes
    (\(worldCharac, worldResults) -> do
      let labelsAndEitherTimeoutsTimes = map
            (\(strategy, seedsResults) ->
              ( fromString $ prettyShowSWCreationStrategy strategy
              , case length seedsResults - length (catMaybes $ map snd seedsResults) of
                  0 -> Right $ average $ map fst seedsResults
                  nSeedsTimeouts -> Left nSeedsTimeouts))
            worldResults

      mapM_ CS.putStrLn $
        showQuantities''
          allowed
          (map snd labelsAndEitherTimeoutsTimes)
          (map fst labelsAndEitherTimeoutsTimes)
          $ fromString $ prettyShowSWCharacteristics worldCharac)
 where
  -- time allowed for each individual seed
  !allowed = fromSecs 0.1-- 100
  !allowedMicros = fromIntegral $ toMicros allowed

maybeToEither :: b -> Maybe a -> Either b a
maybeToEither err = maybe (Left err) Right


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
        (SWCreationStrategy Rotate Cyclic.Order2)
  withNumberedSeed (withDuration . profile props) 0 >>= print


-- | Runs several actions sequentially, allocating a given budget to each.
{-# INLINABLE withinDuration #-}
withinDuration :: (Ord a) => Time Duration System -> (a -> IO b) -> [a] -> IO (Map a (Maybe b))
withinDuration duration act args =
-- Note that for this to work we may need to compile with -fno-omit-yields
  Map.fromList . zip args <$> forM args (timeout micros . act)
 where
  micros = fromIntegral $ toMicros duration



displayRandomValues :: IO [Word32] -> IO ()
displayRandomValues getWord32s =
  replicateM_ 15 $
    map (bool 'Z' ' ' . (>= 0x80000000)) <$> getWord32s >>= putStrLn

displayRandomValuesF :: IO [Float] -> IO ()
displayRandomValuesF get =
  replicateM_ 15 $
    map (bool 'Z' ' ' . (<= 0.5)) <$> get >>= putStrLn

testRNG :: IO ()
testRNG = do
  putStrLn "/dev/urandom"
  displayRandomValues mkSeedSystem

  withDifferentSeeds_ $ \gen -> do
    --s <- save gen
    putStrLn "geni"
    displayRandomValues $ replicateM 256 $ uniform gen
    {-
    gen' <- restore s
    putStrLn "genf"
    displayRandomValuesF $ replicateM 256 $ uniform gen'
    -}


-- | Runs the action several times, with different - deterministically seeded - generators.
withDifferentSeeds :: (GenIO -> IO a) -> IO [a]
withDifferentSeeds act =
  mapM (withNumberedSeed act) [0..10]

withDifferentSeeds_ :: (GenIO -> IO a) -> IO ()
withDifferentSeeds_ = void . withDifferentSeeds

withNumberedSeed :: (GenIO -> IO a) -> Int -> IO a
withNumberedSeed act i = do
  -- we use deterministic seeds made from the same source as 'withSystemRandom' (see https://github.com/bos/mwc-random/issues/64)
  gen <- initialize $ fromList $ deterministicMWCSeeds !! i
  putStrLn $ "- With seed " ++ show (i :: Int) ++ ":"
  act gen

-- | Drops the first measurement.
timeWithDifferentSeeds_ :: (GenIO -> IO a) -> IO [Time Duration System]
timeWithDifferentSeeds_ = fmap (map fst) . timeWithDifferentSeeds

-- | Drops the first measurement.
timeWithDifferentSeeds :: (GenIO -> IO a) -> IO [(Time Duration System, a)]
timeWithDifferentSeeds act =
  drop 1 <$> withDifferentSeeds (withDuration . act)

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
