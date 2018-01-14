{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_HADDOCK hide #-}

-- | This module defines the delta renderer's default policies.

module Imj.Graphics.Render.Delta.DefaultPolicies
           where

import           Imj.Graphics.Color
import           Imj.Graphics.Render.Delta.Types


-- | @=@ 'DynamicSize'
defaultResizePolicy :: ResizePolicy
defaultResizePolicy = DynamicSize

-- | @=@ 'ClearAtEveryFrame'
defaultClearPolicy :: ClearPolicy
defaultClearPolicy = ClearAtEveryFrame

-- | @=@ 'black'
defaultClearColor :: Color8 Background
defaultClearColor = black
