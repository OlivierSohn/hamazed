module Main where

import           Imj.Prelude
import           Prelude(print, putStrLn, putStr, length)
import           Control.Arrow((&&&))
import           Control.Concurrent(threadDelay)
import           Data.IORef(newIORef, atomicModifyIORef', readIORef)
import           Data.List(foldl')
import qualified Data.Map.Strict as Map
import           Data.Maybe(catMaybes)
import           System.IO(hFlush, stdout)
import           System.Random.MWC(create)

import           Imj.Data.Matrix.Cyclic(Matrix, produceUsefulInterleavedVariations, unsafeGet, nrows, ncols)

import           Imj.Game.Hamazed.World.Space.Types
import           Imj.Game.Hamazed.World.Space
import           Imj.Game.Hamazed.World.Space.Read
import           Imj.Timing
import           Imj.Util

-- command used to profile:
-- stack build --executable-profiling --library-profiling --ghc-options="-fprof-auto -rtsopts" && stack exec -- imj-game-hamazed-exe +RTS -M2G -p

main :: IO ()
main =
  profileMkSmallWorld
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
  let d = asDistribution $ catMaybes $ map fst l
  maybe
    (return ())
    (\(min_,_) -> when (min_ <= 1) $ do
      mapM_ id $ map snd l
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
