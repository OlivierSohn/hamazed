{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import           Imj.Prelude
import           Control.Concurrent(forkIO, threadDelay)

import           Imj.Audio
import           Imj.Music

main :: IO ()
main = usingAudio $ do
  inParallel $ -- very audible cracks
    map (playAtTempo 300 simpleInstrument) vivaldi4seasons
  playVoicesAtTempo 300 simpleInstrument vivaldi4seasons
  -- in case the threads have been desynchronized and one finishes a bit later:
  threadDelay 10000

 where

  inParallel [] = return ()
  inParallel (v:vs) = do
    void $ forkIO $ inParallel vs
    v

  vivaldi4seasons =
    map (concat . replicate 4)
    $ map (map (transposeSymbol 4)) $ [voices|
      do -
      vsol -

      mi . mi . mi . ré do sol - - - - . sol fa
      do . do . vsi do vsi vla vsol . vsi . ré . mi ré

      mi . mi . mi . ré do sol - - - - . sol fa
      do . do . vsi do vsi vla vsol . vsi . ré . mi ré

      mi . fa sol fa .   mi .   ré   .   vsi  .   vsol .
      do . ré mi  ré vsi do vla vsol vfa# vsol vfa# vsol .
    |]
