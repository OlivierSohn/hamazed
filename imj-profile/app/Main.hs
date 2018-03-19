module Main where

import            Imj.Prelude
import            Prelude(print)

import            Data.IORef(newIORef, atomicModifyIORef')
import            Data.Maybe(isJust)
import            System.Random.MWC(create)

import            Imj.Game.Hamazed.World.Space.Types
import            Imj.Game.Hamazed.World.Space

-- command used to profile:
-- stack build --executable-profiling --library-profiling --ghc-options="-fprof-auto -rtsopts" && stack exec -- imj-game-hamazed-exe +RTS -M2G -p

main :: IO ()
main = do
  -- use a deterministic random numbers source
  gen <- create

  r <- newIORef (0 :: Int)
  (res, stats) <- mkSmallWorld gen (Size 10 5) OneComponentPerShip (ComponentCount 1) 0.7 $ do
    newValue <- atomicModifyIORef' r (\v -> (succ v, succ v))
    return (newValue /= 20000)
  print stats
  when (isJust res) $ error "result was found"
