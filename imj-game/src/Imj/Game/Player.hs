{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}

module Imj.Game.Player
      ( PlayerEssence(..)
      ) where

import           Imj.Prelude

import           Imj.Game.Status
import           Imj.Graphics.Color.Types
import           Imj.Server.Types

data PlayerEssence = PlayerEssence {
    playerEssenceName :: {-# UNPACK #-} !ClientName
  , playerEssenceStatus :: {-unpack sum-} !PlayerStatus
  , playerEssenceColor :: {-# UNPACK #-} !(Color8 Foreground)
} deriving(Generic, Show)
instance Binary PlayerEssence
