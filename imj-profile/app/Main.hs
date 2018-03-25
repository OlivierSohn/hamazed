module Main where

import           Imj.Prelude
import           Prelude(print, putStrLn, putStr, length)
import           Control.Arrow((&&&))
import           Control.Concurrent(threadDelay)
import           Data.IORef(newIORef, atomicModifyIORef')
import           Data.List(foldl')
import qualified Data.Map.Strict as Map
import           Data.Maybe(isJust, catMaybes)
import           System.IO(hFlush, stdout)
import           System.Random.MWC(create)

import           Imj.Data.Matrix.Cyclic(Matrix, produceUsefullInterleavedVariations, unsafeGet, nrows, ncols)

import           Imj.Game.Hamazed.World.Space.Types
import           Imj.Game.Hamazed.World.Space
import           Imj.Game.Hamazed.World.Space.Read
import           Imj.Util

-- command used to profile:
-- stack build --executable-profiling --library-profiling --ghc-options="-fprof-auto -rtsopts" && stack exec -- imj-game-hamazed-exe +RTS -M2G -p

main :: IO ()
main =
  --profileMkSmallWorld
  --printVariations
  measureMemory

measureMemory :: IO ()
measureMemory = do
  gen <- create -- use a deterministic random numbers source
  forever $ do
    delay "Make random mat"
    mkSmallMat gen 0.7 (Size 1000 1000) >>= \m -> do
      delay "print all" -- Here we have 126 M
      let l = produceUsefullInterleavedVariations m
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

printVariations :: IO ()
printVariations = do
  gen <- create -- use a deterministic random numbers source
  forever $
    map getTopology . produceUsefullInterleavedVariations <$> mkSmallMat gen 0.7 (Size 6 12)
       >>= analyzeDistribution

getTopology :: Matrix Material -> (Maybe ComponentCount, IO ())
getTopology r =
  let res = matchTopology (ComponentCount 1) r
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
  (res, stats) <- mkSmallWorld gen (Size 10 5) (ComponentCount 1) 0.7 $ do
    newValue <- atomicModifyIORef' r (succ &&& succ)
    return (newValue /= 20000)
  print stats
  when (isJust res) $ error "result was found"
