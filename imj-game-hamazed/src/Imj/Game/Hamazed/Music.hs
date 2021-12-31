{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

{-| These musics were written exclusively using 'voice' because
'voices' was not available at that time. -}

module Imj.Game.Hamazed.Music
  ( scoreForLevel
  ) where

import           Imj.Prelude

import           Data.List hiding(transpose, intersperse, intercalate)

import           Imj.Audio
import           Imj.Game.Hamazed.Level

scoreForLevel :: LevelSpec -> Score Instrument
scoreForLevel (LevelSpec n _) =
  hamazedScores !! idx
 where
  nScores = length hamazedScores
  idx = (fromIntegral (n-firstLevel)) `mod` nScores

hamazedScores :: [Score Instrument]
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

primaryScore :: Score Instrument
primaryScore = mkScore organicInstrument
  [ (panCentered, firstVoice)
  , (panCentered, secondVoice)
  , (panCentered, thirdVoice)]
 where
  firstVoice =
    concatMap ((++) begin) variations
   where
    begin = [voice|
      do . .
      . . sol
      re - -
      - mib fa
      sol mib do
      re . v sol
      do . .
      |]

    variations =
      [ [voice|vsol . .|]
      , [voice|vsol vla vsi|]
      , [voice|vsol . .|]
      , [voice|re re re|]
      , [voice|vsol . .|]
      , [voice|vsol vsi re|]
      , [voice|vsol . .|]
      , [voice|re sol vsol|]
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
    re . .
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

secondaryScore :: Score Instrument
secondaryScore =
  mkScore organicInstrument
    [ (panCentered, firstVoice)
    , (panCentered, secondVoice)
    ]
 where

  firstVoice = [voice|
    do . . .
    vsi . . .
    do . . .
    re . . .
    vsi . . .
    do . . .
    re . . .
    mib . . .
    do . . .
    re . . .
    mib . . .
    fa . . .
    re . . .
    mib . . .
    fa . . .
    sol . . .

    lab . . .
    fa . . .
    re . . .
    vsi . . .
    sol . . .
    mib . . .
    do . . .
    re . mib .
    re . . .
    re . . .
    sol . . .
    sol . . .
    sol . . .
    fa . . .
    mib . . .
    re . . .
  |]

  secondVoice = [voice|
    sol - - -
    . . sol -
    lab - sol -
    fa - mib -
    fa - - -
    . . fa -
    sol fa mib re
    do - re -
    mib - - -
    . . do -
    vsi - do -
    re - do -
    vsi - - -
    do - - -
    vsi . do .
    re - mib -

    re - - .
    . . fa -
    sol - lab -
    fa - re -
    mib - sol -
    ^do - ^re -
    ^mib - sol -
    ^do - sol -
    ^fa ^mib ^re ^mib
    ^re ^do si ^do
    si ^do ^re ^do
    si ^do ^re ^mib
    ^do . ^re .
    si . ^do .
    lab . sol .
    fa - - vsol
  |]

tertiaryScore :: Score Instrument
tertiaryScore =
  mkScore organicInstrument
    [ (panCentered, firstVoice ++ secondVoice)
    , (panCentered, concat (replicate (length firstVoice) silence) ++ firstVoice')]
 where
   silence = [voice|.|]

   firstVoice = [voice|
     do re mib fa
     sol . do re
     mib fa sol .
     do re mib fa
     re - - -
     - - - -
     lab . . .
     sol . . .

     ^do sib lab sol
     . . ^do sib
     lab sol . .
     do re mib fa
     sol - - -
     fa - - -
     mib - - -
     v sol . . .
     |]


   firstVoice' = [voice|
     do re mib fa
     sol . do .
     mib . sol .
     do re mib fa
     re - - -
     - - - -
     lab . . .
     sol . . sol

     . sol sol .
     . . . .
     . . . .
     do re mib fa
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
     mib . re .

     do mib re fa
     mib . do mib
     re sol mib lab
     sol . . ^do
     si ^do ^re .
     sol lab sol ^mib
     ^re ^do si .
     si . sol .
   |]

quatScore :: Score Instrument
quatScore =
  mkScore organicInstrument
    [ (panCentered, voice1)
    , (panCentered, bass1)
    , (panCentered, bass2)
    , (panCentered, bass3)
    ]
 where

  voice1 = firstVoice ++ firstVoice ++ firstVoice2

  firstVoice = [voice|
  ^do . ^sol
  ^re . .
  ^mib ^re ^do
  ^re . .
  |]

  firstVoice2 = [voice|
  ^fa ^mib ^re
  ^mib ^re ^do
  ^fa ^mib ^re
  ^mib ^re ^do
  ^mib ^fa ^sol
  ^do ^re ^mib
  ^do ^re ^mib
  ^re . .
  |]

  bass1 = [voice|
  mib . .
  fa . .
  do re mib
  fa . .
  mib . .
  fa . .
  do re mib
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

quintScore :: Score Instrument
quintScore =
  mkScore bellInstrument
    [ (panCentered, v2)
    , (panCentered, (take (3*12) (cycle v1)) ++ take (3*12) (cycle v1'))
    , (panCentered, (take (3*12) (cycle b1)) ++ take (3*12) (cycle b1'))
    , (panCentered, (take (3*12) (cycle b2)) ++ take (3*12) (cycle b2'))
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
    . ^re ^mib ^fa . . ^mib . .
    ^re . . . . . . . .
    . ^mib ^fa ^sol . . ^fa . .
    ^mib . . . . . . ^sol .
    ^mib sol lab sib ^do ^re ^mib . .
    ^mib . . . . . . . ^re
    ^mib sol lab sib ^do ^re ^mib . .
  |]

sextScore :: Score Instrument
sextScore =
  mkScore organicInstrument
    [ (panCentered, take (length v2) $ cycle v1)
    , (panCentered, v2)
    , (panCentered, take (length v2) $ cycle $ skip1 ++ v3)
    , (panCentered, take (length v2) $ cycle (acc n1))
    , (panCentered, take (length v2) $ cycle (acc n2))
    , (panCentered, take (length v2) $ cycle (acc n3))
    ]
 where
  v1 = [voice|
      . . vmi . vsol . vsi . re - - do# . vla . vsi
      . . vmi . vsol . vsi . mi . re . do# - - -
      . . mi . sol . mi . re - - dod . vla . vsi
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
    . re mi sol mi re sol . . . . . . . . .
    . . . . . . . . . . . . . . . .
    . re mi re . . . . . mi re sol . . . .
    . . . . . . . . . . . . . . . .
    . re mi re . . . . . . . . . . . .
    . . . . . . . . . . . . . . . .
    . re . re . mi re sol . . . . . . . . .
    . . . . . . . . . . . . . . .
    |]

  n1 = [voice|si|]
  n2 = [voice|^re|]
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

sevthScore :: Score Instrument
sevthScore =
  mkScore organicInstrument
    [ (panCentered, v1 ++ v1)
    , (panCentered, v2 ++ v2)
    , (panCentered, v3 ++ v3)
    , (panCentered, silence ++ v4)
    , (panCentered, silence ++ v5)
    ]
 where
   v1 = [voice|
   vsol do mib . . re mib . . . . .
   vfa vsib re . . mib re . . . . .
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
   . . . . . . sol . re mib do .
   . . . . . . re . mib . fa .
   |]

   v5 = [voice|
   . . . . . . . . . . . .
   . . . . . . vsi . do . re .
   |]

   silence = [voice|
   . . . . . . . . . . . .
   . . . . . . . . . . . .
   |]


heighthScore :: Score Instrument
heighthScore = mkScore organicInstrument
  [ (panCentered, concat $ replicate 8 melody)
  , (panCentered, concat $ replicate 4 bass)
  , (panCentered, silence ++ upperVoice ++ silence ++ upperVoice)
  , (panCentered, silence ++ silence ++ silence ++ upperVoice2)
  ]

 where

  melody = [voice|
    si . . sol . . mi .
    . . sol . la . si .
    ^do . . fad . . re .
    . . fad . sol . la .
    |]

  bass = [voice|
    mi . . . . . . .
    . . . . . . . .
    re . . . . . . .
    . . . . . . . mi
    . . . . . . . .
    . . . . . . . re
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


ninthScore :: Score Instrument
ninthScore = mkScore shortInstrument
  [ (panCentered, melody)
  , (panCentered, bass1)
  , (panCentered, bass2)
  , (panCentered, concat $ replicate 4 bass3)
  ]

 where

  melody = [voice|
    la . . . . . ^do ^re ^mi . la .
    lab . . . . . ^mib ^mi ^mib ^mi ^mib ^mi
    sol . . . . . ^do ^re ^mi . sol .
    fad . . . . . ^mi ^mib ^re ^reb ^do si
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
    re . . . . . . . . . . .
    re . . . . . . . . . . .
  |]
  bass3 = [voice|
    . . mi . . . . . . . . .
  |]

tenthScore :: Score Instrument
tenthScore = mkScore
  shortInstrument
  [ (panCentered, concat $ replicate 2 melody)
  , (panCentered, bass)
  ]

 where

  melody = [voice|
    ^do si la . la si ^do ^re ^mi . . .
    ^re ^do si . . . ^do si la . . .
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

eleventhScore :: Score Instrument
eleventhScore = mkScore
  testInstrument
  [ (panCentered, melody)
  , (panCentered, melody2)
  , (panCentered, bass)
  ]

 where

  melody = [voice|
    ^do ^reb ^do ^reb ^do .
    ^do ^reb ^do ^reb ^do .
    ^do ^reb ^do ^reb ^do ^reb
    ^do ^reb ^do ^reb ^do .
    ^do ^reb ^do ^reb ^do .
    ^do ^reb ^do ^reb ^do .
    ^do ^reb ^do ^reb ^do ^reb
    ^do sib ^do ^reb ^do .
    ^do ^reb ^do ^reb ^do .
    ^do ^reb ^do ^reb ^do .
    ^do ^reb ^do ^reb ^do ^reb
    ^do ^reb ^do ^reb ^do .
    ^do ^reb ^do ^reb ^do .
    ^do ^reb ^do ^reb ^do ^reb
    ^do . . ^reb ^do ^reb
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

twelvthScore :: Score Instrument
twelvthScore = Score
  [ mkVoice stringsInstrument panCentered $ concat $ replicate 8 melody
  , mkVoice stringsInstrument panCentered $ concat $ replicate 8 bass1
  , mkVoice stringsInstrument panCentered $ concat $ replicate 8 bass2
  , mkVoice shortInstrument panCentered $ concat (replicate 4 $ mute melody) ++ concat (replicate 4 voice1)
  , mkVoice longInstrument panCentered $ concat (replicate 4 $ mute melody) ++ concat (replicate 4 voice2)
  ]

 where

  melody = [voice|
    do re fa sol - - - -
    do re re - - - - -
  |]

  bass1 = [voice|
    . . vfa - - - - -
    . . vmi - - - - -
  |]

  bass2 = [voice|
    . . vla - - - - -
    . . vsol - - - - -
  |]

  voice1 = [voice|
    . . . . ^mi - ^do -
    - - - - - - - -
  |]

  voice2 = [voice|
    . . ^mi - - - - -
    - - - - - - - -
  |]
