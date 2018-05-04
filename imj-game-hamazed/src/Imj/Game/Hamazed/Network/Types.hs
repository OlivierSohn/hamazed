{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Imj.Game.Hamazed.Network.Types
      ( HamazedClientEvent(..)
      , HamazedServerEvent(..)
      , HamazedClient
      -- * Client / Server communication
      , WorldRequestArg(..)
      , SharedEnumerableValueKey(..)
      , SharedValue(..)
      , GameNotif(..)
      , GameStep(..)
      , GameStatus(..)
      -- * Game
      , GameStateEssence(..)
      , ShotNumber(..)
      , Operation(..)
      ) where

import           Imj.Game.Hamazed.Network.Internal.Types
