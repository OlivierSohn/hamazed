{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}


module Imj.Game.Hamazed.Music
  ( primaryScore
  , secondaryScore
  , tertiaryScore
  , alarm
  ) where

import           Imj.Prelude

import           Data.List
import           Imj.Music

primaryScore :: Score
primaryScore = mkScore [firstVoice, secondVoice, thirdVoice]
 where
  firstVoice =
    concatMap ((++) begin) variations
   where
    -- alternating chords Dom, Solm
    -- on Laser, we could either make a tone-less sound, or a chord / note in the right tonality.
    -- Maybe 3 notes in rapid succession (the server would need to have a "Thread" to schedule laser sounds).
    begin = [notes|
      do . .
      . . sol
      ré - -
      - mib fa
      sol mib do
      ré . v sol
      do . .
      |]

    variations =
      [ [notes|vsol . .|]
      , [notes|vsol vla vsi|]
      , [notes|vsol . .|]
      , [notes|ré ré ré|]
      , [notes|vsol . .|]
      , [notes|vsol vsi ré|]
      , [notes|vsol . .|]
      , [notes|ré sol vsol|]
      ]

  secondVoice =
    [notes|
    . . sol
    . . .
    . . sol
    . . .
    mib . .
    fa . .
    mib . .
    ré . .
    |]

  thirdVoice =
    [notes|
    . . .
    mib . .
    . . .
    fa . .
    . . .
    . . .
    . . .
    . . .
    |]

secondaryScore :: Score
secondaryScore =
  mkScore
    [ firstVoice
    , secondVoice
    ]
 where

  firstVoice = [notes|
    do . . .
    vsi . . .
    do . . .
    ré . . .
    vsi . . .
    do . . .
    ré . . .
    mib . . .
    do . . .
    ré . . .
    mib . . .
    fa . . .
    ré . . .
    mib . . .
    fa . . .
    sol . . .

    lab . . .
    fa . . .
    ré . . .
    vsi . . .
    sol . . .
    mib . . .
    do . . .
    ré . mib .
    ré . . .
    ré . . .
    sol . . .
    sol . . .
    sol . . .
    fa . . .
    mib . . .
    ré . . .
  |]

  secondVoice = [notes|
    sol - - -
    . . sol -
    lab - sol -
    fa - mib -
    fa - - -
    . . fa -
    sol - fa -
    mib - ré -
    mib - - -
    . . do -
    vsi - do -
    ré - mib -
    ré - - -
    do - - -
    vsi . do .
    ré - mib -

    fa - - -
    . . fa -
    sol - lab -
    fa - ré -
    mib - sol -
    ^do - ^ré -
    ^mib - sol -
    ^do - sol -
    ^fa ^mib ^ré ^mib
    ^ré ^do si ^do
    si ^do ^ré ^do
    si ^do ^ré ^mib
    ^do . ^ré .
    si . ^do .
    lab . sol .
    fa - - vsol
  |]

tertiaryScore :: Score
tertiaryScore =
  mkScore
    [ firstVoice ++ secondVoice
    , concat (replicate (length firstVoice) silence) ++ firstVoice']
 where
   silence = [notes|.|]

   firstVoice = [notes|
     do ré mib fa
     sol . do ré
     mib fa sol .
     do ré mib fa
     ré - - -
     - - - -
     lab . . .
     sol . . .

     ^do sib lab sol
     . . ^do sib
     lab sol . .
     do ré mib fa
     sol - - -
     fa - - -
     mib - - -
     v sol . . .
     |]


   firstVoice' = [notes|
     do ré mib fa
     sol . do .
     mib . sol .
     do ré mib fa
     ré - - -
     - - - -
     lab . . .
     sol . . sol

     . sol sol .
     . . . .
     . . . .
     do ré mib fa
     sol - - -
     fa - - -
     mib - - -
     vsol . . .
     |]

   secondVoice = [notes|
     mib fa sol lab
     sol . sol lab
     sol fa sol .
     sol fa mib fa
     . . sol .
     vsol . sol .
     . . sol .
     mib . ré .

     do mib ré fa
     mib . do mib
     ré sol mib lab
     sol mib do ^do
     si ^do ^ré .
     sol lab sol ^mib
     ^ré ^do si .
     si . sol .
   |]

alarm :: [Symbol]
alarm =
  [notes|do ^do|]
