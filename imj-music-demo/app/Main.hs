{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import           Control.Concurrent(forkIO, threadDelay)
import           Control.Monad(void)

import           Imj.Music

main :: IO ()
main = usingAudio2 $ do
  inParallel $
    map
      (playAtTempo 120 simpleInstrument)
      [ [notes|do ré mi|]
      , [notes|mi fa sol|]
      ]
  -- in case the threads have been desynchronized and one finishes a bit later:
  threadDelay 10000

 where

  inParallel [] = return ()
  inParallel (v:vs) = do
    void $ forkIO $ inParallel vs
    v

{-
  [voices|
    do ré mi
    mi fa sol
   |]
-}
