{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}


module Imj.Game.Hamazed
      ( -- * The game
        {-| In Hamazed, you are a 'BattleShip' pilot surrounded by flying 'Number's.

        Your mission is to shoot exactly the 'Number's whose sum will equate the
        current 'Level' 's /target number/.

        The higher the 'Level' (1..12), the more 'Number's are flying around (up-to 16).
        And the smaller the 'World' gets.

        Good luck !

        /Note that to adapt the keyboard layout, you can modify 'eventFromKey'./
        -}
          run
        -- * Game loop
        {-| Hamazed is a /synchronous/, /event-driven/ program. Its /simplified/ main loop is:

        * 'getNextDeadline'

            * \(deadline\) = the next foreseen 'Deadline'.

        * 'getEventForMaybeDeadline'

            * \(event\) =

                * a key-press occuring /before/ \(deadline\) expires
                * or the \(deadline\) event

        * 'update'

            * Update 'GameState' according to \(event\)

        * 'render'

            * Render (using "Imj.Graphics.Render.Delta" to avoid
            <https://en.wikipedia.org/wiki/Screen_tearing screen tearing>).
        -}
      , getNextDeadline
      , getEventForMaybeDeadline
      , update
      , render
        -- * Deadlines
      , Deadline(..)
      , DeadlineType(..)
        -- * Timestamped Events
        -- | Every 'Event' is timestamped with the time at which it was generated:
      , TimestampedEvent(..)
        -- * Events
      , Event(..)
      , ActionTarget(..)
      , MetaAction(..)
        -- * GameState
        {-| 'GameState' has two fields of type 'World' : during 'Level' transitions,
        we render the /old/ 'World' while using the /new/ 'World' 's
        dimensions to animate the UI accordingly (see "Imj.Graphics.UI.Animation"). -} -- TODO this could be done differently
      , GameState(..)
        -- * Environment
        {- | -}
      , module Imj.Game.Hamazed.Env
        -- * Keyboard layout
      , eventFromKey
        -- * Reexport
      , module Imj.Game.Hamazed.World
      , UIAnimation(..)
      ) where

import           Imj.Prelude

import           Imj.Game.Hamazed.Color
import           Imj.Game.Hamazed.Env
import           Imj.Game.Hamazed.KeysMaps
import           Imj.Game.Hamazed.Level
import           Imj.Game.Hamazed.Level.Types
import           Imj.Game.Hamazed.Loop.Deadlines
import           Imj.Game.Hamazed.Loop.Event
import           Imj.Game.Hamazed.Loop.Render
import           Imj.Game.Hamazed.Loop.Run
import           Imj.Game.Hamazed.Loop.Timing
import           Imj.Game.Hamazed.Loop.Update
import           Imj.Game.Hamazed.Parameters
import           Imj.Game.Hamazed.Types
import           Imj.Game.Hamazed.World
