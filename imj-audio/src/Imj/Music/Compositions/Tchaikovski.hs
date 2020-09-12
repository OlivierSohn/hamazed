{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Music.Compositions.Tchaikovski
      ( tchaikovskiSwanLake
      ) where

import           Imj.Prelude
import           Data.List(replicate, concat)

import           Imj.Music.Instruction
import           Imj.Music.Alter
import           Imj.Music.Compose

-- | The beginning of
-- <https://www.youtube.com/watch?v=f9NnI3u9ppY&index=4&list=RDRPdelxGwnnI Act 1 Scene 2, dance of the cygnets>,
-- of Pyotr Ilyich Tchaikovski's Swan Lake.
tchaikovskiSwanLake :: (Double, [[Instruction]])
tchaikovskiSwanLake = (bpm, part)
 where
  bpm = 700
  part = map (map (transposeSymbol 5)) $
    concatSystems $
      [[voices|vsol . . . vdo . . .|]
      ,map (concat . replicate 2) [voices|
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
