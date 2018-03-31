{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import           Imj.Prelude
import           Prelude(print, putStrLn, putStr, length, writeFile)
import           Control.Arrow((&&&))
import           Control.Concurrent(threadDelay)
import           Data.IORef(newIORef, atomicModifyIORef', readIORef)
import           Data.List(foldl', unlines, intersperse)
import qualified Data.Map.Strict as Map
import           Data.Vector(fromList)
import qualified Data.Vector.Unboxed as U(toList)
import           Data.Word(Word32)
import           System.IO(hFlush, stdout)
import           System.Random.MWC

import qualified Imj.Data.Matrix.Cyclic as Cyclic

import           Imj.Game.Hamazed.World.Space.Types
import           Imj.Game.Hamazed.World.Space
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
main = do
  profileLargeWorld'
  --profileLargeWorld
  --profileMkSmallWorld
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
    in foldl'
        (\s i ->
          foldl'
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
getTopology r@(SmallMatInfo _ w) =
  let res = matchTopology NCompsNotRequired (ComponentCount 1) r
      compCount = getComponentCount res
      render = do
        print compCount
        putStrLn ""
        mapM_ putStrLn $ showInBox $ writeWorld w
        putStrLn ""
  in (compCount, render)

profileMkSmallWorld :: IO ()
profileMkSmallWorld = do
  -- use a deterministic random numbers source
  gen <- create
  r <- newIORef (0 :: Int)
  (duration, (res, stats)) <- withDuration $
    mkSmallWorld gen (Size 18 9) (ComponentCount 1) 0.8 $ do
      newValue <- atomicModifyIORef' r (succ &&& succ)
      return (newValue /= 200)
  putStrLn $ prettyShowStats stats
  print duration -- min   without stats, 4.75 with stats
  case res of
    NeedMoreTime -> return ()
    Impossible err -> error $ "impossible :" ++ show err
    Success _ -> readIORef r >>= \iteration -> error $ "result found at iteration " ++ show iteration

profile :: GenIO -> IO (MkSpaceResult SmallWorld, Statistics)
profile gen = do
  (res, stats) <- mkSmallWorld gen
    --(Size 32 72) (ComponentCount 1) 0.2
    --(Size  8 18) (ComponentCount 1) 0.5
    --(Size  8 18) (ComponentCount 1) 0.6
    (Size  8 18) (ComponentCount 1) 0.7
    $ pure True -- never interrupt
  case res of
    NeedMoreTime -> error "test logic"
    Impossible err -> error $ "impossible :" ++ show err
    Success _ -> return ()
  return (res, stats)

profileLargeWorld' :: IO ()
profileLargeWorld' =
  withNumberedSeed profile 0 >>= print

profileLargeWorld :: IO ()
profileLargeWorld = do
  timesAndResStats <- timeWithDifferentSeeds profile
  let stats = map (snd . snd) timesAndResStats
      --results = map (fst . fst) timesAndResStats
      times = map fst timesAndResStats
  mapM_ (putStrLn . prettyShowStats) stats
  mapM_ putStrLn $ showQuantities times

prettyShowTimes :: [Time Duration System] -> [String]
prettyShowTimes times' =
  showArrayN
    (Just ["Seed", "Duration", "Duration (us)"])
    $ map
        (\(a,b,c) ->
        let n = round $ b * (40 :: Float)
        in [ show a
           , replicate n '|'
           , showTime c
           ])
        $ zip3 [1 :: Int ..] ratioTimes times'
 where
  times = map toMicros times'
  maxTime = fromMaybe 1 $ maximumMaybe times
  ratioTimes = map (\t -> fromIntegral t / fromIntegral maxTime) times


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
timeWithDifferentSeeds_ :: (Show a) => (GenIO -> IO a) -> IO [Time Duration System]
timeWithDifferentSeeds_ = fmap (map fst) . timeWithDifferentSeeds

-- | Drops the first measurement.
timeWithDifferentSeeds :: (Show a) => (GenIO -> IO a) -> IO [(Time Duration System, a)]
timeWithDifferentSeeds act =
  drop 1 <$> withDifferentSeeds (\gen ->
    withDuration (act gen) >>= \res -> do
      print res
      return res)

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
      allNums <- unlines . map ((++) "    " . concat) . splitEvery 8 . words . intercalate ", " . map show <$> mkSeedSystem
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
