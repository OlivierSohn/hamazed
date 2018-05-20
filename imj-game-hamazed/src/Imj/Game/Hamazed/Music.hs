{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Imj.Game.Hamazed.Music
  ( scoreForLevel
  , alarm
  ) where

import           Imj.Prelude

import           Data.List hiding(transpose, intersperse)

import           Imj.Game.Hamazed.Level
import           Imj.Music.Alter
import           Imj.Music.Compose
import           Imj.Music.Types

scoreForLevel :: LevelSpec -> Score
scoreForLevel (LevelSpec n _) =
  hamazedScores !! idx
 where
  nScores = length hamazedScores
  idx = (fromIntegral (n-firstLevel)) `mod` nScores

hamazedScores :: [Score]
hamazedScores =
  [ transpose 2 primaryScore
  , transpose 2 secondaryScore
  , transpose 2 tertiaryScore
  , transpose (0) $ intersperse Rest quatScore
  , transpose (-2) quintScore
  ]

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
    sol fa mib ré
    do - ré -
    mib - - -
    . . do -
    vsi - do -
    ré - do -
    vsi - - -
    do - - -
    vsi . do .
    ré - mib -

    ré - - .
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

quatScore :: Score
quatScore =
  mkScore
    [ voice1
    , bass1
    , bass2
    , bass3
    ]
 where

  voice1 = firstVoice ++ firstVoice ++ firstVoice2

  firstVoice = [notes|
  ^do . ^sol
  ^ré . .
  ^mib ^ré ^do
  ^ré . .
  |]

  firstVoice2 = [notes|
  ^fa ^mib ^ré
  ^mib ^ré ^do
  ^fa ^mib ^ré
  ^mib ^ré ^do
  ^mib ^fa ^sol
  ^do ^ré ^mib
  ^do ^ré ^mib
  ^ré . .
  |]

  bass1 = [notes|
  mib . .
  fa . .
  do ré mib
  fa . .
  mib . .
  fa . .
  do ré mib
  sol . .
  sib . .
  la . .
  lab sol solb
  sol . .
  sib . .
  la . .
  fa . .
  sol . .
  |]

  bass2 = [notes|
  lab . .
  sol . .
  solb . .
  sol . .
  lab . .
  sol . .
  solb . .
  la . .
  . . .
  . . .
  ^do . .
  ^do . .
  ^do . .
  . . .
  lab . .
  si . .
  |]


  bass3 = [notes|
  . . .
  . . .
  lab . .
  . . .
  . . .
  . . .
  la . .
  la . .
  . . .
  . . .
  . . .
  . . .
  . . .
  . . .
  . . .
  . . .
  |]

quintScore :: Score
quintScore =
  mkScore
    [ v2
    , (take (3*12) (cycle v1)) ++ take (3*12) (cycle v1')
    , (take (3*12) (cycle b1)) ++ take (3*12) (cycle b1')
    , (take (3*12) (cycle b2)) ++ take (3*12) (cycle b2')
    ]
 where

  b1 = [notes|mib . .|]
  b2 = [notes|lab . .|]
  v1 = [notes|^do . .|]

  b1' = [notes|mib . .|]
  b2' = [notes|sol . .|]
  v1' = [notes|^do . .|]

  v2 = [notes|
    ^do . . . . . . . .
    . ^ré ^mib ^fa . . ^mib . .
    ^ré . . . . . . . .
    . ^mib ^fa ^sol . . ^fa . .
    ^mib . . . . . . ^sol .
    ^mib sol lab sib ^do ^ré ^mib . .
    ^mib . . . . . . . ^ré
    ^mib sol lab sib ^do ^ré ^mib . .
  |]


alarm :: [Symbol]
alarm =
  [notes|do ^do|]
