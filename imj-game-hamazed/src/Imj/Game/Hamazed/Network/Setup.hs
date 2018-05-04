{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Imj.Game.Hamazed.Network.Setup
      ( changeWorldShape
      ) where

import           Imj.Prelude

import           Imj.Game.Hamazed.World.Types

changeWorldShape :: WorldShape -> WorldParameters -> Maybe WorldParameters
changeWorldShape d p
  | d == worldShape p = Nothing
  | otherwise = Just $ p { worldShape = d }
