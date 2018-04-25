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
  [ [notes|vsi vla vsol|]
  , [notes|vsol vla vsi|]
  , [notes|sol lab sol|]
  , [notes|fa mib ré|]
  , [notes|ré . v sol|]
  , [notes|ré mib ré|]
  , [notes|vsol ré sol|]
  , [notes|ré sol vsol|]
  ]

alarm :: [Symbol]
alarm =
  [notes|do ^do|]
