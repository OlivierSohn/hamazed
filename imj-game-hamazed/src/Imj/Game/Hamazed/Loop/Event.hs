{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}

module Imj.Game.Hamazed.Loop.Event
    ( isVisible
    , isPrincipal
    -- * Reexports
    , module Imj.Game.Hamazed.Loop.Event.Types
    ) where

import           Imj.Prelude

import           Imj.Game.Hamazed.Types
import           Imj.Game.Hamazed.Loop.Event.Types

-- | Tells if after handling an 'Event' we should render or not.
isVisible :: Event -> Bool
isVisible = \case
  (Action Ship _) -> False -- When the ship accelerates, nothing changes visually
  _ -> True

-- | No 2 principal events can be part of the same update group.
-- it allows to separate important game action on different rendered frames.
isPrincipal :: Event -> Bool
isPrincipal = \case
  (Timeout (Deadline _ _ AnimateParticleSystems)) -> False
  (Timeout (Deadline _ _ DisplayContinueMessage)) -> False
  (Timeout (Deadline _ _ AnimateUI)) -> False
  _ -> True
