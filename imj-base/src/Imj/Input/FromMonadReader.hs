{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Input.FromMonadReader
       ( -- * Player input
       playerEndsProgram
       -- * Reexports
       , MonadReader, Int64
       ) where

import           Imj.Prelude

import           Control.Monad.Reader.Class(MonadReader, asks)

import           Imj.Input.Types

{-# INLINABLE playerEndsProgram #-}
playerEndsProgram :: (PlayerInput i, MonadReader i m, MonadIO m)
                  => m Bool
playerEndsProgram =
  join(asks programShouldEnd)
