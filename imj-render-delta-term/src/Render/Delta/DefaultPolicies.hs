{-# OPTIONS_HADDOCK hide #-}

-- | This module defines the default policies.

module Render.Delta.DefaultPolicies
           where


import System.IO(BufferMode(..))

import Render.Delta.Types
import Color


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
