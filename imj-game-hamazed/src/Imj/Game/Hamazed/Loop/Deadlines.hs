{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}

module Imj.Game.Hamazed.Loop.Deadlines
    ( getNextDeadline
    ) where

import           Imj.Prelude

import           Data.List( minimumBy, find )
import           Data.Maybe( catMaybes )

import           Imj.Game.Hamazed.Level
import           Imj.Game.Hamazed.Loop.Event.Types
import           Imj.Game.Hamazed.Types
import           Imj.Graphics.UI.Animation
import           Imj.Graphics.Animation.Design.Update
import           Imj.Timing


{- | Returns the next 'Deadline' to handle.

We prefer having time-accurate game motions for central items of the game
(the 'BattleShip', the 'Number's) than having time-accurate explosive 'Animation's.

Hence, when multiple overdue deadlines are competing, the following priorities apply
(higher number = higher priority):

\[
\newcommand\T{\Rule{0pt}{.5em}{.3em}}
  \begin{array}{|c|c|c|}
	\hline
  \textbf{ Priority } \T & \textbf{ Name     } \T & \textbf{ Description                            } \\\hline
	\text{ 5 } & \text{ AnimateUI              } \T & \text{ Inter-level animations                   } \\\hline
	\text{ 4 } & \text{ DisplayContinueMessage } \T & \textit{ Press a key to continue                } \\\hline
  \text{ 3 } & \text{ MoveFlyingItems        } \T & \text{ Move the BattleShip and Numbers          } \\\hline
  \text{ 2 } & \textit{ Player event         } \T & \text{ Handle a key-press                       } \\\hline
  \text{ 1 } & \text{ Animate                } \T & \text{ Update animations (explosions and others)} \\\hline
	\end{array}
\]

When no 'Deadline' is overdue, we return the closest one in time, irrespective
of its priority.
-}
{-We /could/ apply priorities for non-overdue deadlines, too. For example if a
'MoveFlyingItems' very closely follows an 'Animate' (say, 15 millisecond after),
we could swap their order so as to have a better guarantee that the game motion
will happen in-time and not be delayed by a potentially heavy animation update.
But it's very unlikely that it will make a difference, except if updating
the 'Animation's becomes /very/ slow for some reason.
-}
getNextDeadline :: GameState
                -- ^ Current state
                -> SystemTime
                -- ^ The current time.
                -> Maybe Deadline
getNextDeadline s t =
  let l = getDeadlinesByDecreasingPriority s t
  in  overdueDeadline t l <|> earliestDeadline' l

earliestDeadline' :: [Deadline] -> Maybe Deadline
earliestDeadline' [] = Nothing
earliestDeadline' l  = Just $ minimumBy (\(Deadline t1 _) (Deadline t2 _) -> compare t1 t2 ) l

overdueDeadline :: SystemTime -> [Deadline] -> Maybe Deadline
overdueDeadline t = find (\(Deadline (KeyTime t') _) -> t' < t)

-- | priorities are : uiAnimation > message > game > player key > animation
getDeadlinesByDecreasingPriority :: GameState -> SystemTime -> [Deadline]
getDeadlinesByDecreasingPriority s@(GameState _ _ _ _ level _) t =
  catMaybes [ uiAnimationDeadline s
            , messageDeadline level t
            , getMoveFlyingItemsDeadline s
            , animationDeadline s
            ]

getMoveFlyingItemsDeadline :: GameState -> Maybe Deadline
getMoveFlyingItemsDeadline (GameState nextGameStep _ _ _ (Level _ _ levelFinished) _) =
  maybe
    (maybe
      Nothing
      (\s -> Just $ Deadline s MoveFlyingItems)
        nextGameStep)
    (const Nothing)
      levelFinished

animationDeadline :: GameState -> Maybe Deadline
animationDeadline (GameState _ world _ _ _ _) =
  maybe Nothing (\ti -> Just $ Deadline ti Animate) $ earliestAnimationDeadline world

uiAnimationDeadline :: GameState -> Maybe Deadline
uiAnimationDeadline (GameState _ _ _ _ _ uianim) =
  maybe
    Nothing
    (\deadline -> Just $ Deadline deadline AnimateUI)
      $ getUIAnimationDeadline uianim

-- | Returns the earliest 'BoundedAnimation' deadline.
earliestAnimationDeadline :: World -> Maybe KeyTime
earliestAnimationDeadline (World _ _ _ animations _) =
  earliestDeadline animations
