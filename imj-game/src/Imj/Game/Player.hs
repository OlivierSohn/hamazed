{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}

module Imj.Game.Player
      ( PlayerEssence(..)
      ) where

import           Imj.Prelude

import           Imj.Network
import           Imj.Game.Status
import           Imj.Graphics.Color.Types

data PlayerEssence = PlayerEssence {
    playerEssenceName :: {-# UNPACK #-} !(ClientName Approved)
  , playerEssenceStatus :: {-unpack sum-} !PlayerStatus
  , playerEssenceColor :: {-# UNPACK #-} !(Color8 Foreground)
} deriving(Generic, Show)
instance Binary PlayerEssence
