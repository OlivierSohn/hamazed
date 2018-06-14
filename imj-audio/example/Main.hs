{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import           Control.Concurrent(threadDelay)

import           Imj.Audio

main :: IO ()
main = usingAudio $ do
  playVoicesAtTempo 300 simpleInstrument vivaldi4seasons

  threadDelay 10000

 where

  -- rearranged by me :)
  vivaldi4seasons =
    map (concat . replicate 4)
    $ map (map (transposeSymbol 4)) $ [voices|
      do -
      vsol -

      mi . mi . mi  .  ré  do  sol - -   - -  . sol fa
      do . do . vsi do vsi vla vsi . .   . ré . mi  ré

      mi . mi . mi . ré do sol ré sol - - mi sol fa
      do . do . vsi do vsi vla vsi vsol vsi do ré . mi ré

      mi . fa sol fa .   mi .   ré   .   vsi  .   vsol .
      do . ré mi  ré vsi do vla vsol vfa# vsol vfa# vsol .

      do -
      vsol -

      mi . mi . mi  .  ré  do  sol -   -  -  -  . sol fa
      do . do . vsi do vsi vla vsi - do do ré . mi  ré
      .  . .  . .   .  .   .   .   sol . sol .  ^do .

      mi sol mi . mi  .  ré  do  sol la   sol -  -  la sol fa
      do .   do . vsi do vsi vla vsi vsol vsi do ré .  mi  ré

      mi . fa sol fa .   mi .   ré   .   vsi  .   vsol .
      do . ré mi  ré vsi do vla vsol vfa# vsol vfa# vsol .
    |]
