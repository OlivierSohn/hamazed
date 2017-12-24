{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_HADDOCK hide #-}

-- | This module defines the default policies.

module Imj.Render.Delta.DefaultPolicies
           where

import           Imj.Prelude

import           System.IO(BufferMode(..))

import           Imj.Color
import           Imj.Render.Delta.Types


-- | @=@ 'MatchTerminalSize'
defaultResizePolicy :: ResizePolicy
defaultResizePolicy = MatchTerminalSize

-- | @=@ 'ClearAtEveryFrame'
defaultClearPolicy :: ClearPolicy
defaultClearPolicy = ClearAtEveryFrame

-- | @=@ 'black'
defaultClearColor :: Color8 Background
defaultClearColor = black

-- | @=@ 'BlockBuffering' $ 'Just' 'maxBound'
defaultStdoutMode :: BufferMode
defaultStdoutMode =
  BlockBuffering $ Just maxBound -- maximize the buffer size to avoid screen tearing
