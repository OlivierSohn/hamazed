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
  [ [notes|sol mib v si|]
  , [notes|sol fa mib|]
  , [notes|sol lab sol|]
  , [notes|fa mi réb|]
  , [notes|ré . v sol|]
  , [notes|ré mib ré|]
  , [notes|sol sol sol|]
  , [notes|ré sol vsol|]
  ]

alarm :: [Symbol]
alarm =
  [notes|do ^do|]
