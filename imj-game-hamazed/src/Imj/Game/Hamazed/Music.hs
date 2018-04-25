{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}


module Imj.Game.Hamazed.Music
  ( mainTheme
  , secondVoice
  , alarm
  ) where

import           Data.List
import           Imj.Music


mainTheme :: [Symbol]
mainTheme =
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

variations :: [[Symbol]]
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

secondVoice :: [Symbol]
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

alarm :: [Symbol]
alarm =
  [notes|do ^do|]
