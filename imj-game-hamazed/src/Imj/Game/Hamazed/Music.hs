{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Imj.Game.Hamazed.Music
  ( scoreForLevel
  , alarm
  ) where

import           Imj.Prelude

import           Data.List hiding(transpose, intersperse, intercalate)

import           Imj.Game.Hamazed.Level
import           Imj.Music.Alter
import           Imj.Music.Compose
import           Imj.Music.Instruments
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
  , intersperse Rest quatScore
  , transpose (-2) quintScore
  , sextScore
  , intersperse Rest $ transpose 12 sevthScore
  , heighthScore
  , ninthScore
  , intersperse Rest tenthScore
  , eleventhScore
  , intercalate [Extend,Extend] twelvthScore
  ]

primaryScore :: Score
primaryScore = mkScore organicInstrument
  [ firstVoice
  , secondVoice
  , thirdVoice]
 where
  firstVoice =
    concatMap ((++) begin) variations
   where
    begin = [voice|
      do . .
      . . sol
      ré - -
      - mib fa
      sol mib do
      ré . v sol
      do . .
      |]

    variations =
      [ [voice|vsol . .|]
      , [voice|vsol vla vsi|]
      , [voice|vsol . .|]
      , [voice|ré ré ré|]
      , [voice|vsol . .|]
      , [voice|vsol vsi ré|]
      , [voice|vsol . .|]
      , [voice|ré sol vsol|]
      ]

  secondVoice =
    [voice|
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
    [voice|
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
  mkScore organicInstrument
    [ firstVoice
    , secondVoice
    ]
 where

  firstVoice = [voice|
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

  secondVoice = [voice|
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
  mkScore organicInstrument
    [ firstVoice ++ secondVoice
    , concat (replicate (length firstVoice) silence) ++ firstVoice']
 where
   silence = [voice|.|]

   firstVoice = [voice|
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


   firstVoice' = [voice|
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

   secondVoice = [voice|
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
     sol . . ^do
     si ^do ^ré .
     sol lab sol ^mib
     ^ré ^do si .
     si . sol .
   |]

quatScore :: Score
quatScore =
  mkScore organicInstrument
    [ voice1
    , bass1
    , bass2
    , bass3
    ]
 where

  voice1 = firstVoice ++ firstVoice ++ firstVoice2

  firstVoice = [voice|
  ^do . ^sol
  ^ré . .
  ^mib ^ré ^do
  ^ré . .
  |]

  firstVoice2 = [voice|
  ^fa ^mib ^ré
  ^mib ^ré ^do
  ^fa ^mib ^ré
  ^mib ^ré ^do
  ^mib ^fa ^sol
  ^do ^ré ^mib
  ^do ^ré ^mib
  ^ré . .
  |]

  bass1 = [voice|
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

  bass2 = [voice|
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


  bass3 = [voice|
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
  mkScore bellInstrument
    [ v2
    , (take (3*12) (cycle v1)) ++ take (3*12) (cycle v1')
    , (take (3*12) (cycle b1)) ++ take (3*12) (cycle b1')
    , (take (3*12) (cycle b2)) ++ take (3*12) (cycle b2')
    ]
 where

  b1 = [voice|mib . .|]
  b2 = [voice|lab . .|]
  v1 = [voice|^do . .|]

  b1' = [voice|mib . .|]
  b2' = [voice|sol . .|]
  v1' = [voice|^do . .|]

  v2 = [voice|
    ^do . . . . . . . .
    . ^ré ^mib ^fa . . ^mib . .
    ^ré . . . . . . . .
    . ^mib ^fa ^sol . . ^fa . .
    ^mib . . . . . . ^sol .
    ^mib sol lab sib ^do ^ré ^mib . .
    ^mib . . . . . . . ^ré
    ^mib sol lab sib ^do ^ré ^mib . .
  |]

sextScore :: Score
sextScore =
  mkScore organicInstrument
    [ take (length v2) $ cycle v1
    , v2
    , take (length v2) $ cycle $ skip1 ++ v3
    , take (length v2) $ cycle (acc n1)
    , take (length v2) $ cycle (acc n2)
    , take (length v2) $ cycle (acc n3)
    ]
 where
  v1 = [voice|
      . . vmi . vsol . vsi . ré - - do# . vla . vsi
      . . vmi . vsol . vsi . mi . ré . do# - - -
      . . mi . sol . mi . ré - - dod . vla . vsi
      . . vmi . vsol . vsi . vla . vsol . vmi - - -
   |]
  v2 = [voice|
      . . . . . . . . . . . . . . . .
      . . . . . . . . . . . . . . . .
      . . . . . . . . . . . . . . . .
      . . . . . . . . . . . . . . . .
      . . . . si si . . la . . sol . . . .
      . . . . si si . . la . . sol . . . .
      . . la . si si . . la . . sol . . . .
      . . la . si si . mi sol sol . . . . . .
      . . . . . . . . . . . . . . . .
      . . . . . . . . . . . . . . . .
      . . . . . . . . . . . . . . . .
      . . . . . . . . . . . . . . . .
      . . . . si si . . la . . sol . . . .
      . . . . si si . . la . . sol . . . .
      . . la . si si . . la . . sol . . . .
      . . la . si si . mi sol sol . . . . . .
   |]

  skip1 = [voice|
        . . . . . . . . . . . . . . . .
        . . . . . . . . . . . . . . . .
        . . . . . . . . . . . . . . . .
        . . . . . . . . . . . . . . . .
        . . . . . . . . . . . . . . . .
        . . . . . . . . . . . . . . . .
        . . . . . . . . . . . . . . . .
        . . . . . . . . . . . . . . . .
        |]

  v3 =
    [voice|
    . ré mi sol mi ré sol . . . . . . . . .
    . . . . . . . . . . . . . . . .
    . ré mi ré . . . . . mi ré sol . . . .
    . . . . . . . . . . . . . . . .
    . ré mi ré . . . . . . . . . . . .
    . . . . . . . . . . . . . . . .
    . ré . ré . mi ré sol . . . . . . . . .
    . . . . . . . . . . . . . . .
    |]

  n1 = [voice|si|]
  n2 = [voice|^ré|]
  n3 = [voice|^mi|]

  acc x =
     [voice|
     . . . . . . . . . . . . . . . .
     . . . . . . . . . . . . . . . .
     . . . . . . . . . . . . . . . .
     . . . . . . . . . . . . . . . .|]
     ++ acc' x
     ++ acc' x

  acc' x = [voice|
     . . . . . . . .      .|]      ++ x ++ [voice|.      .           . . . .
     . . . . . . . .|] ++ x ++ [voice|-           .|] ++ x ++ [voice|. . . .|] -- it would be nice to have a live coding app to try these things

sevthScore :: Score
sevthScore =
  mkScore organicInstrument
    [ v1 ++ v1
    , v2 ++ v2
    , v3 ++ v3
    , silence ++ v4
    , silence ++ v5
    ]
 where
   v1 = [voice|
   vsol do mib . . ré mib . . . . .
   vfa vsib ré . . mib ré . . . . .
   |]

   v2 = [voice|
   . . . . . . vlab . . . . .
   . . . . . . vla . . . . .
   |]


   v3 = [voice|
   . . . . . . vsib . . . . .
   . . . . . . vsi . . . . .
   |]

   v4 = [voice|
   . . . . . . sol . ré mib do .
   . . . . . . ré . mib . fa .
   |]

   v5 = [voice|
   . . . . . . . . . . . .
   . . . . . . vsi . do . ré .
   |]

   silence = [voice|
   . . . . . . . . . . . .
   . . . . . . . . . . . .
   |]


heighthScore :: Score
heighthScore = mkScore organicInstrument
  [ concat $ replicate 8 melody
  , concat $ replicate 4 bass
  , silence ++ upperVoice ++ silence ++ upperVoice
  , silence ++ silence ++ silence ++ upperVoice2
  ]

 where

  melody = [voice|
    si . . sol . . mi .
    . . sol . la . si .
    ^do . . fad . . ré .
    . . fad . sol . la .
    |]

  bass = [voice|
    mi . . . . . . .
    . . . . . . . .
    ré . . . . . . .
    . . . . . . . mi
    . . . . . . . .
    . . . . . . . ré
    . . . . . . . .
    mi . . . . . . .
    |]

  silence = [voice|
    . . . . . . . .
    . . . . . . . .
    . . . . . . . .
    . . . . . . . .
    . . . . . . . .
    . . . . . . . .
    . . . . . . . .
    . . . . . . . .
    |]
  upperVoice = [voice|
    ^mi . . ^mi . . ^sol ^sol
    . . ^mi . . si . .
    ^fad . . ^fad . . . .
    . . . . . . . .
    ^mi . . ^mi . . . .
    . . . . . . . .
    ^fad . . ^fad . . . .
    . . . . . . . .
    |]

  upperVoice2 = [voice|
    ^si . . ^si . . . .
    . . . . . . . .
    . . . . . . . .
    . . . . . . . .
    ^si . . ^si . . . .
    . . . . . . . .
    . . . . . . . .
    . . . . . . . .
    |]


ninthScore :: Score
ninthScore = mkScore shortInstrument
  [ melody
  , bass1
  , bass2
  , concat $ replicate 4 bass3
  ]

 where

  melody = [voice|
    la . . . . . ^do ^ré ^mi . la .
    lab . . . . . ^mib ^mi ^mib ^mi ^mib ^mi
    sol . . . . . ^do ^ré ^mi . sol .
    fad . . . . . ^mi ^mib ^ré ^réb ^do si
    |]

  bass1 = [voice|
    do . . . . . . . . . . .
    vsi . . . . . . . . . . .
    vsib . . . . . . . . . . .
    vla . . . . . . . . . . .
  |]

  bass2 = [voice|
    mi . . . . . . . . . . .
    mi . . . . . . . . . . .
    ré . . . . . . . . . . .
    ré . . . . . . . . . . .
  |]
  bass3 = [voice|
    . . mi . . . . . . . . .
  |]

tenthScore :: Score
tenthScore = mkScore
  shortInstrument
  [ concat $ replicate 2 melody
  , bass
  ]

 where

  melody = [voice|
    ^do si la . la si ^do ^ré ^mi . . .
    ^ré ^do si . . . ^do si la . . .
    |]

  bass = [voice|
    . . vla do mi vla
    do mi vsol do mi vsol
    do mi vfa vla do vfa
    vla do vmi vla do vmi
    . . vla do mi vla
    do mi vsol do mi vsol
    do mi vfa vla do vfa
    vla do vmi vla vdo vmi
  |]

eleventhScore :: Score
eleventhScore = mkScore
  testInstrument
  [ melody
  , melody2
  , bass
  ]

 where

  melody = [voice|
    ^do ^réb ^do ^réb ^do .
    ^do ^réb ^do ^réb ^do .
    ^do ^réb ^do ^réb ^do ^réb
    ^do ^réb ^do ^réb ^do .
    ^do ^réb ^do ^réb ^do .
    ^do ^réb ^do ^réb ^do .
    ^do ^réb ^do ^réb ^do ^réb
    ^do sib ^do ^réb ^do .
    ^do ^réb ^do ^réb ^do .
    ^do ^réb ^do ^réb ^do .
    ^do ^réb ^do ^réb ^do ^réb
    ^do ^réb ^do ^réb ^do .
    ^do ^réb ^do ^réb ^do .
    ^do ^réb ^do ^réb ^do ^réb
    ^do . . ^réb ^do ^réb
    ^do . . . . .
  |]

  melody2 = [voice|
    . . . . . .
    . . . . . .
    ^mi ^fa ^mi ^fa ^mi ^fa
    ^mi ^fa ^sol ^fa ^mi .
    . . . . . .
    . . . . . .
    ^mi ^fa ^mi ^fa ^mi ^fa
    ^mi ^fa ^mi ^fa ^mi .
    . . . . . .
    . . . . . .
    ^mi ^fa ^mi ^fa ^mi ^fa
    ^mi ^fa ^sol ^fa ^mi .
    . . . . . .
    . . . . . .
    ^mi . . ^fa ^mi ^fa
    ^mi . . . . .
  |]

  bass = [voice|
    do . sol . sib .
    do . fa . lab .
    do . mi . sol .
    lab . sol . mi .
    do . sol . sib .
    do . fa . lab .
    do . mi . sol .
    lab . sol . mi .
    do . sol . sib .
    do . fa . lab .
    do . mi . sol .
    lab . sol . mi .
    do . sol . sib .
    do . fa . lab .
    do do mi . sol .
    lab . sol . mi .
    |]

twelvthScore :: Score
twelvthScore = Score
  [ mkVoice stringsInstrument $ concat $ replicate 8 melody
  , mkVoice stringsInstrument $ concat $ replicate 8 bass1
  , mkVoice stringsInstrument $ concat $ replicate 8 bass2
  , mkVoice shortInstrument $ concat (replicate 4 $ mute melody) ++ concat (replicate 4 voice)
  , mkVoice longInstrument $ concat (replicate 4 $ mute melody) ++ concat (replicate 4 voice2)
  ]

 where

  melody = [voice|
    do ré fa sol - - - -
    do ré ré - - - - -
  |]

  bass1 = [voice|
    . . vfa - - - - -
    . . vmi - - - - -
  |]

  bass2 = [voice|
    . . vla - - - - -
    . . vsol - - - - -
  |]

  voice = [voice|
    . . . . ^mi - ^do -
    - - - - - - - -
  |]

  voice2 = [voice|
    . . ^mi - - - - -
    - - - - - - - -
  |]

alarm :: [VoiceInstruction]
alarm =
  [voice|do ^do|]
