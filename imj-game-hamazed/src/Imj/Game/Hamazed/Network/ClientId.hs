{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Imj.Game.Hamazed.Network.ClientId
      ( checkName
      , mkClientColorFromCenter
      , makePlayerName
      , checkNameAvailability
      ) where

import           Imj.Prelude

import           Control.Monad.IO.Class(MonadIO)
import           Control.Monad.State.Strict(MonadState, gets)
import           Data.Char (isPunctuation, isSpace)
import           Data.Text(pack, unpack)

import           Imj.Game.Hamazed.Network.Internal.Types
import           Imj.Game.Hamazed.Network.Types
import           Imj.Graphics.Color.Types
import           Imj.Server.Internal.Types

import           Imj.Graphics.Color

checkName :: PlayerName -> Either Text ()
checkName (PlayerName name) =
  if any ($ unpack name) [ null, any isPunctuation, any isSpace]
    then
      Left "Name cannot contain punctuation or whitespace, and cannot be empty"
    else
      Right ()

mkClientColorFromCenter :: ClientId -> Color8 Foreground -> Color8 Foreground
mkClientColorFromCenter (ClientId i) ref =
  let nColors = countHuesOfSameIntensity ref
      -- we want the following mapping:
      -- 0 -> 0
      -- 1 -> 1
      -- 2 -> -1
      -- 3 -> 2
      -- 4 -> -2
      -- ...
      dist = quot (succ i) 2
      n' = fromIntegral dist `mod` nColors
      n = if odd i then n' else -n'
  in rotateHue (fromIntegral n / fromIntegral nColors) ref

makePlayerName :: (MonadIO m, MonadState (ServerState HamazedServerState) m)
               => SuggestedPlayerName -> m PlayerName
makePlayerName (SuggestedPlayerName sn) = do
  let go mayI = do
        let proposal = PlayerName $ pack $ maybe sn ((++) sn . show) mayI
        checkNameAvailability proposal >>= either
          (\_ -> go $ Just $ maybe (2::Int) succ mayI)
          (\_ -> return proposal)
  go Nothing

checkNameAvailability :: (MonadIO m, MonadState (ServerState HamazedServerState) m)
                      => PlayerName -> m (Either Text ())
checkNameAvailability name =
  any ((== name) . getName . unClient) <$> gets clientsMap >>= \case
    True  -> return $ Left "Name is already taken"
    False -> return $ Right ()
