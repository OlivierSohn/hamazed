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
import           Options.Applicative(long, help, flag)

import           Imj.Arg.Class
import           Imj.Audio
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
  withAudio (WithAudio yes)
    | yes = usingAudio
    | otherwise = id

  triggerLaserSound (WithAudio useAudio)
    | useAudio = liftIO laserSound
    | otherwise = return ()

  playMusic (WithAudio useAudio) mus instr
    | useAudio = liftIO $ play mus instr
    | otherwise = return ()

  -- WARNING when changing this, also change 'parseArg'
  defaultAudio = enabledAudio

  {-# INLINABLE triggerLaserSound #-}
  {-# INLINABLE defaultAudio #-}
  {-# INLINABLE playMusic #-}
  {-# INLINABLE withAudio #-}
instance Arg WithAudio where
  -- WARNING when changing this, also change 'defaultAudio'
  parseArg = Just $
    flag enabledAudio disabledAudio
      (  long "silent"
      <> help
      "[Client] disables music and audio effects."
      )

enabledAudio :: WithAudio
enabledAudio = WithAudio True

disabledAudio :: WithAudio
disabledAudio = WithAudio False
