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
import           Data.Word(Word32)
import           Foreign.Marshal.Alloc   (allocaBytes)
import           Foreign.Marshal.Array   (peekArray)
import           System.IO(IOMode(..), hGetBuf, hFlush, stdout, withBinaryFile)
import           System.Random.MWC(create, initialize, GenIO)

import           Imj.Data.Matrix.Cyclic(Matrix, produceUsefulInterleavedVariations, unsafeGet, nrows, ncols)

import           Imj.Game.Hamazed.World.Space.Types
import           Imj.Game.Hamazed.World.Space
import           Imj.Timing
import           Imj.Util

import           Imj.Random.MWC.Seeds

-- command used to profile:
-- stack build --executable-profiling --library-profiling --ghc-options="-fprof-auto -rtsopts" && stack exec -- imj-game-hamazed-exe +RTS -M2G -p

main :: IO ()
main = do
  --writeSeedsSource
  profileLargeWorld
  --profileMkSmallWorld
  --printVariations2
  --printVariations
  --measureMemory

measureMemory :: IO ()
measureMemory = do
  gen <- create -- use a deterministic random numbers source
  forever $ do
    delay "Make random mat"
    unsafeMkSmallMat gen 0.7 (Size 1000 1000) >>= \m -> do
      delay "print all" -- Here we have 126 M
      let l = produceUsefulInterleavedVariations m
      print (length l, map matrixSum l)
      delay "Produce, getTopology, analyze " -- Still 126 M
      mapM
        (\x -> do
          putStr "." >> flush
          return $ getTopology x) l >>= analyzeDistribution -- ~ 300 M
 where
  flush = hFlush stdout
  delay nextAction = do
    putStr "\nWaiting ... "
    forM_ [0..9::Int] $ \i -> do
      threadDelay $ 1 * 1000 * 1000
      putStr (show i) >> flush
    putStrLn $ '\n' : nextAction
  matrixSum mat =
    let n = nrows mat
        m = ncols mat
    in foldl'
        (\s i ->
          foldl'
            (\s' j -> s' + bool 0 1 (Air == unsafeGet i j mat))
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

printVariations2 :: IO ()
printVariations2 = do
  gen <- create -- use a deterministic random numbers source
  replicateM_ 1 $
    produceUsefulInterleavedVariations <$> unsafeMkSmallMat gen 0.8 (Size 18 9)
       >>= mapM_ (mapM_ putStrLn . showInBox . writeWorld)

printVariations :: IO ()
printVariations = do
  gen <- create -- use a deterministic random numbers source
  forever $
    map getTopology . produceUsefulInterleavedVariations <$> unsafeMkSmallMat gen 0.8 (Size 18 9)
       >>= analyzeDistribution

getTopology :: Matrix Material -> (Maybe ComponentCount, IO ())
getTopology r =
  let res = matchTopology DontForceComputeComponentCount (ComponentCount 1) r
      compCount = getComponentCount res
      render = do
        print compCount
        putStrLn ""
        mapM_ putStrLn $ showInBox $ writeWorld r
        putStrLn ""
  in (compCount, render)

profileMkSmallWorld :: IO ()
profileMkSmallWorld = do
  -- use a deterministic random numbers source
  gen <- create
  r <- newIORef (0 :: Int)
  ((res, stats), duration) <- withDuration $
    mkSmallWorld gen (Size 18 9) (ComponentCount 1) 0.8 $ do
      newValue <- atomicModifyIORef' r (succ &&& succ)
      return (newValue /= 200)
  putStrLn $ prettyShowStats stats
  print duration -- min   without stats, 4.75 with stats
  case res of
    NeedMoreTime -> return ()
    Impossible err -> error $ "impossible :" ++ show err
    Success _ -> readIORef r >>= \iteration -> error $ "result found at iteration " ++ show iteration

profileLargeWorld :: IO ()
profileLargeWorld = do
  timesAndResStats <- timeWithDifferentSeeds $ \gen -> do
    -- I could wait until I have x successes
    (res, stats) <- mkSmallWorld gen (Size 36 72) (ComponentCount 1) 0.2 $ return True -- never interrupt
    case res of
      NeedMoreTime -> error "test logic"
      Impossible err -> error $ "impossible :" ++ show err
      Success _ -> return ()
    return (res, stats)
  let stats = map (snd . fst) timesAndResStats
      --results = map (fst . fst) timesAndResStats
      times = map snd timesAndResStats
  mapM_ (putStrLn . prettyShowStats) stats
  mapM_ putStrLn $ prettyShowTimes times

prettyShowTimes :: [Time Duration System] -> [String]
prettyShowTimes times' =
  showArrayN
    (Just ["Seed", "Duration", "Duration'"])
    $ map
        (\(a,b,c) ->
        let n = round $ b * (40 :: Float)
        in [ show a
           , replicate n '|'
           , show c
           ])
        $ zip3 [1 :: Int ..] ratioTimes times
 where
  times = map toMicros times'
  maxTime = fromMaybe 1 $ maximumMaybe times
  ratioTimes = map (\t -> fromIntegral t / fromIntegral maxTime) times

-- | Runs the action several times, with different - deterministically seeded - generators.
withDifferentSeeds :: (GenIO -> IO a) -> IO [a]
withDifferentSeeds act =
  -- we use deterministic seeds made from the same source as 'withSystemRandom' (see https://github.com/bos/mwc-random/issues/64)
  mapM (initialize . fromList >=> act) $ take 11 deterministicMWCSeeds

-- | Drops the first measurement.
timeWithDifferentSeeds_ :: (Show a) => (GenIO -> IO a) -> IO [Time Duration System]
timeWithDifferentSeeds_ act = map snd <$> timeWithDifferentSeeds act

-- | Drops the first measurement.
timeWithDifferentSeeds :: (Show a) => (GenIO -> IO a) -> IO [(a, Time Duration System)]
timeWithDifferentSeeds act =
  drop 1 <$> withDifferentSeeds (\gen -> do
    putStrLn "-"
    withDuration (act gen) >>= \res@(a,time) -> do
      print a
      print time
      return res)

writeSeedsSource :: IO ()
writeSeedsSource =
  mkSeedsSourceFile 100 >>= writeFile "./imj-profile/src/Imj/Random/MWC/Seeds.hs"

mkSeedsSourceFile :: Int -> IO String
mkSeedsSourceFile n = do
  allSeeds <- (\x -> [" [", unlines $ intersperse " ," x, " ]"]) <$>
    replicateM n (do
      allNums <- unlines . map ((++) "    " . concat) . splitEvery 8 . words . intercalate ", " . map show <$> acquireSeedSystem
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

acquireSeedSystem :: IO [Word32] -- TODO remove once exported by mwc-random
acquireSeedSystem = do
  -- Read 256 random Word32s from /dev/urandom
  let nbytes = 1024
      random = "/dev/urandom"
  allocaBytes nbytes $ \buf -> do
    nread <- withBinaryFile random ReadMode $
               \h -> hGetBuf h buf nbytes
    peekArray (nread `div` 4) buf
