module Main where

import           Imj.Prelude
import           Prelude(print, putStrLn)

import           Data.IORef(newIORef, atomicModifyIORef')
import qualified Data.Map.Strict as Map
import           Data.Maybe(isJust, catMaybes)
import           System.Random.MWC(create)

import           Imj.Data.Matrix.Cyclic( toLists,
                                      -- mapRotationsM
                                       mapInterleavedVariationsM
                                      )

import           Imj.Game.Hamazed.World.Space.Types
import           Imj.Game.Hamazed.World.Space
import           Imj.Util

-- command used to profile:
-- stack build --executable-profiling --library-profiling --ghc-options="-fprof-auto -rtsopts" && stack exec -- imj-game-hamazed-exe +RTS -M2G -p

main :: IO ()
main =
  --profileMkSmallWorld
  printVariations

printVariations :: IO ()
printVariations = do
  gen <- create -- use a deterministic random numbers source
  forever $
   mkSmallMat gen 0.7 (Size 6 12) >>=
      mapMatVariations getTopology >>= analyzeDistribution

 where
  -- NOTE in the game, we use a mixed approach : interleaving, and if we are close to cc target number, rotate.
  --mapMatVariations = mapRotationsM -- doesn't change the number of cc very much
  mapMatVariations = mapInterleavedVariationsM -- changes the number of cc a lot

  getTopology r = do
    let res = matchTopology (ComponentCount 1) r
        compCount = getComponentCount res
        render = do
          print compCount
          putStrLn ""
          mapM_ putStrLn $ showInBox $ map (map toChar) $ toLists r
          putStrLn ""
    return [(compCount, render)]

  analyzeDistribution l = do
    let d = asDistribution $ catMaybes $ map fst l
    maybe
      (return ())
      (\(min_,_) -> when (min_ <= 1) $ do
        mapM_ id $ map snd l
        mapM_ putStrLn (showDistribution d)
        putStrLn "")
      $ Map.lookupMin d

-- unlike in the game, we draw Air to better see the component shapes
toChar :: Material -> Char
toChar Air = 'O'
toChar Wall = ' '

profileMkSmallWorld :: IO ()
profileMkSmallWorld = do
  -- use a deterministic random numbers source
  gen <- create
  r <- newIORef (0 :: Int)
  (res, stats) <- mkSmallWorld gen (Size 10 5) (ComponentCount 1) 0.7 $ do
    newValue <- atomicModifyIORef' r (\v -> (succ v, succ v))
    return (newValue /= 20000)
  print stats
  when (isJust res) $ error "result was found"
