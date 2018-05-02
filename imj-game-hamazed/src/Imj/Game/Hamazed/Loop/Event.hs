{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}

module Imj.Game.Hamazed.Loop.Event
    ( isPrincipal
    , mkEmptyGroup
    , visible
    , count
    , tryGrow
    ) where

import           Imj.Prelude
import           Prelude(length)

import           Imj.Event
import           Imj.Game.Hamazed.Loop.Timing
import           Imj.Graphics.ParticleSystem.Design.Timing
