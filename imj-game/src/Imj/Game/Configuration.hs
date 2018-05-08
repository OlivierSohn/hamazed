{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Imj.Game.Configuration
      ( BackendType(..)
      , Debug(..)
      , ServerOnly(..)
      , WithAudio(..)
      ) where

import           Imj.Prelude
import           Imj.Game.Audio.Class
import           Imj.Game.Sound
import           Imj.Music

data BackendType =
    Console
  | OpenGLWindow
  deriving (Show)

newtype Debug = Debug Bool
  deriving (Show)

newtype ServerOnly = ServerOnly Bool

newtype WithAudio = WithAudio Bool
  deriving (Show)
instance Audio WithAudio where
  triggerLaserSound (WithAudio useAudio)
    | useAudio = liftIO laserSound
    | otherwise = return ()
  playMusic (WithAudio useAudio) mus instr
    | useAudio = liftIO $ play mus instr
    | otherwise = return ()
  {-# INLINABLE triggerLaserSound #-}
  {-# INLINABLE playMusic #-}
