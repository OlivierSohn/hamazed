{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Game.Hamazed
      ( -- * The game
        {-| In Hamazed, you are a 'BattleShip' pilot surrounded by flying 'Number's.

        Your mission is to shoot exactly the 'Number's whose sum will equate the
        current 'Level' 's /target number/.

        The higher the 'Level' (1..12), the more 'Number's are flying around (up-to 16).
        And the smaller the 'World' gets.

        Good luck !

        /Note that to adapt the keyboard layout, you can modify 'translatePlatformEvent'./
        -}
        run
      -- * Parameters
      {-| When the game starts, the player can chose 'World' parameters:

      * 'WorldShape' : square or rectangular 'World' where the width is twice the height
      * 'WallDistribution' : Should the 'World' have walls, and what kind of walls.
      * 'ViewMode' : Should the view be centered on the 'BattleShip' or not.
       -}
      , WorldParameters(..)
      , ViewMode(..)
        -- * Game loop
        {-| Hamazed is a /synchronous/, /event-driven/ program. Its /simplified/ main loop is:

        * 'produceEvent'

            * creates the next \(event\) to handle.

        * 'updateAppState'

            * Update according to \(event\)

        * Draw and render using "Imj.Graphics.Render.Delta" to avoid
        <https://en.wikipedia.org/wiki/Screen_tearing screen tearing:

            * 'draw' : draws the game elements.
            * 'renderToScreen' : renders what was drawn to the screen.
        -}
      , produceEvent
      , updateAppState
      , draw
        -- * Deadlines
        {-| Deadlines are ordered on a timeline, and are associated to events.

        [visibility]
        A /visible/ deadline may produce a visible effect when its associated event is used to update the game.
        Hence, when a visible deadline is addressed, we want to render as soon as possible
        to keep the rendering responsive w.r.t the game state. But if we do a render for every
        visible deadline, under heavy conditions, when many visible deadlines are very close to one another
        (e.g. many heavy particle systems are running at the same time),
        we won't have enough CPU / GPU resources to ensure that the game and animations runs at the normal speed.
        To address this:

        * We introduce the notion of priority : /non-important/ deadlines can be ignored
        when more important deadlines need to be handled. Hence,
        particle systems could seem to be slower than usual when a lot of them are running,
        but the game itself will always run at the same speed.
        * Some overdue deadlines are grouped and addressed in the same loop,
        so that only one render is needed to reflect the change induced by all of them.
        The followings constraints apply:

          * To keep a good time-detail of game updates, deadlines that produce visible
          /game/ changes are placed in distinct groups:
          for example, a player induced laser shot and a game step
          won't be in the same group, but a game step and many particle systems updates can
          be in the same group.

          * To keep a stable render rate, we limit the duration used to update grouped deadlines to 10 ms.

          * We ensure that no 2 visible deadlines that are separated by more than the period of
          particle system animation are in the same group. This ensures than every update
          of a given particle system happens in a distinct group.

        -}
      , Deadline(..)
      , DeadlineType(..)
        -- * Events
      , Event(..)
      , ActionTarget(..)
      , MetaAction(..)
        -- * GameState
      , GameState(..)
        -- * Keyboard layout
      , translatePlatformEvent
        -- * Reexport
      , module Imj.Game.Hamazed.Env
      , module Imj.Game.Hamazed.World
      , UIAnimation(..)
      ) where

import           Imj.Prelude

import           Imj.Game.Hamazed.Color
import           Imj.Game.Hamazed.Env
import           Imj.Game.Hamazed.KeysMaps
import           Imj.Game.Hamazed.Level.Types
import           Imj.Game.Hamazed.Loop.Draw
import           Imj.Game.Hamazed.Loop.Event
import           Imj.Game.Hamazed.Loop.Run
import           Imj.Game.Hamazed.Loop.Timing
import           Imj.Game.Hamazed.Loop.Update
import           Imj.Game.Hamazed.Types
import           Imj.Game.Hamazed.World
