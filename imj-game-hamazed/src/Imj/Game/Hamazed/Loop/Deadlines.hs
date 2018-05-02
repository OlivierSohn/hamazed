{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}

module Imj.Game.Hamazed.Loop.Deadlines
    ( getNextDeadline
    , TypedDeadline(..)
    ) where

import           Imj.Prelude
import qualified Data.Map.Strict as Map

import           Data.List( minimumBy, sortBy)
import           Data.Maybe( mapMaybe )

import           Imj.Game.Hamazed.Loop.Event.Priorities
import           Imj.Game.Hamazed.Types
import           Imj.Game.Hamazed.State.Types
import           Imj.Graphics.ParticleSystem.Design.Update

import           Imj.Event
import           Imj.Game.Hamazed.Loop.Timing


{- | Returns the next 'Deadline' to handle.

We prefer having time-accurate game motions for central items of the game
(the 'BattleShip', the 'Number's) than having time-accurate explosive 'ParticleSystem's.

Hence, when multiple overdue deadlines are competing, the following priorities apply
(higher number = higher priority):

\[
\newcommand\T{\Rule{0pt}{.5em}{.3em}}
  \begin{array}{|c|c|c|}
	\hline
  \textbf{ Priority } \T & \textbf{ Name     } \T & \textbf{ Description                            } \\\hline
	\text{ 6 } & \text{ UI update              } \T & \text{ Inter-level animations                   } \\\hline
	\text{ 5 } & \text{ Text messages          } \T & \textit{ 'Press a key to continue', etc...      } \\\hline
	\text{ 4 } & \text{ Laser particle-system  } \T & \textit{ Updates a 'laser shot' particle system } \\\hline
  \text{ 3 } & \text{ Game step              } \T & \text{ Move the BattleShip and Numbers          } \\\hline
  \text{ 2 } & \textit{ Player input         } \T & \text{ Handle a key-press                       } \\\hline
  \text{ 1 } & \text{ Default particle-system} \T & \text{ Updates other particle systems (explosions, etc...)} \\\hline
	\end{array}
\]

When no 'Deadline' is overdue, we return the closest one in time, irrespective
of its priority.
-}
{-We /could/ apply priorities for non-overdue deadlines, too. For example if a
'MoveFlyingItems' very closely follows an 'AnimateParticleSystem' (say, 15 millisecond after),
we could swap their order so as to have a better guarantee that the game motion
will happen in-time and not be delayed by a potentially heavy animation update.
But it's very unlikely that it will make a difference, except if updating
the 'ParticleSystem's becomes /very/ slow for some reason.
-}
{-# INLINE getNextDeadline #-}
getNextDeadline :: (GameLogic g, MonadState (AppState g) m)
                => Time Point System
                -- ^ The current time.
                -> m (Maybe TypedDeadline)
getNextDeadline t =
  gets game >>= \g -> do
    let l = getDeadlinesByDecreasingPriority g
        overdues = filter (\(Deadline t' _ _) -> t' < t) l
    return $! case overdues of
          [] ->
            Future <$> earliestDeadline' l
          highPriorityOverdue:_ ->
            Just $ Overdue highPriorityOverdue

data TypedDeadline = Future {-# UNPACK #-} !Deadline
                   | Overdue {-# UNPACK #-} !Deadline
                   deriving(Generic, Show)


earliestDeadline' :: [Deadline] -> Maybe Deadline
earliestDeadline' [] = Nothing
earliestDeadline' l  = Just $ minimumBy (\(Deadline t1 _ _) (Deadline t2 _ _) -> compare t1 t2 ) l


{-# INLINE getDeadlinesByDecreasingPriority #-}
getDeadlinesByDecreasingPriority :: (GameLogic g) => Game g -> [Deadline]
getDeadlinesByDecreasingPriority (Game _ _ g dcs _ _ _ _ _) =
  -- sort from highest to lowest priority
  sortBy (\(Deadline _ p1 _) (Deadline _ p2 _) -> compare p2 p1) $
    getDeadlines g ++
    (getParticleSystemsDeadlines $ getParticleSystems g) ++
    stateAnimDeadlines dcs
 where
  getParticleSystemsDeadlines =
    map (\(key, Prioritized p a) ->
          Deadline (particleSystemTimePointToSystemTimePoint $ getDeadline a) p
            $ AnimateParticleSystem key)
      . Map.toList

stateAnimDeadlines :: [(a,AnimatedLine)] -> [Deadline]
stateAnimDeadlines =
  mapMaybe
    (\(_,AnimatedLine _ _ mayD) -> mayD)
