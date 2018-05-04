{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Imj.Game.Internal.Types
      ( GameArgs(..)
      , BackendType(..)
      ) where

import           Imj.Prelude

import           Imj.Server.Class
import           Imj.ServerView.Types

import           Imj.Game.Types
import           Imj.Graphics.Font
import           Imj.Graphics.Render.Delta.Backend.OpenGL(PreferredScreenSize(..))

data BackendType =
    Console
  | OpenGLWindow
  deriving (Show)

data GameArgs g = GameArgs
  !Bool
  !(Maybe ServerName)
  !(Maybe ServerPort)
  !(Maybe ServerLogs)
  !(Maybe (ServerConfigT (ServerT g)))
  !(Maybe (ConnectIdT (ServerT g)))
  !(Maybe BackendType)
  !(Maybe PPU)
  !(Maybe PreferredScreenSize)
  !Bool
