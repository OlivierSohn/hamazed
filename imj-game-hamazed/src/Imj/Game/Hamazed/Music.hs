{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}


module Imj.Game.Hamazed.Music
  ( mainTheme
  , alarm
  ) where

import           Data.List
import           Imj.Music


mainTheme :: [Symbol]
mainTheme =
  concatMap ((++) begin) variations
 where
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

alarm :: [Symbol]
alarm =
  [notes|do ^do|]
