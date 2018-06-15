{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Music.Compositions.Tchaikowski
      ( tchaikowskiSwanLake
      ) where

import           Imj.Prelude
import           Data.List(replicate, concat)

import           Imj.Music.CTypes
import           Imj.Music.Alter
import           Imj.Music.Compose

-- | Based on a piece of Pyotr Ilyich Tchaikowski's Swan Lake.
tchaikowskiSwanLake :: (Float, [[VoiceInstruction]])
tchaikowskiSwanLake = (bpm, part)
 where
  bpm = 700
  part = map (map (transposeSymbol 5)) $
    concatSystems $
      [[voices|vsol . . . vdo . . .|]
      ,map (concat . replicate 4) [voices|
    mib  . . . mib . . . mib  . . . mib - - - - - ré mib fa . . . mib . . . ré . . .
    vsol . . . vdo . . . vsol . . . vdo . . . vsol . . . vdo . . . vsol . . . vdo . . .

    fa  . . . fa . . . fa  . . . fa - - - - - mib fa sol . . . fa . . . mib . . .
    vsol . . . vdo . . . vsol . . . vdo . . . vsol . . . vdo . . . vsol . . . vdo . . .

    sol  . . . ^do . . . si  . . . sol - - - - - - . sol . fa . mib . ré . do . . .
    vsol . . . vdo . . . vsol . . . vdo . . . vsol . . . vdo . . . vsol . . . vdo . . .

    sol  . . . ^do . . . si  . . . sol - - - - - - . sol . fa . mib . ré . do . . .
    vsol . . . vdo . . . vsol . . . vdo . . . vsol . . . vdo . . . vsol . . . vdo . . .
    |]
    ]
