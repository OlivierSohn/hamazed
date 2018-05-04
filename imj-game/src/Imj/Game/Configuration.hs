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

data BackendType =
    Console
  | OpenGLWindow
  deriving (Show)

newtype Debug = Debug Bool
  deriving (Show)

newtype ServerOnly = ServerOnly Bool

newtype WithAudio = WithAudio Bool
  deriving (Show)
