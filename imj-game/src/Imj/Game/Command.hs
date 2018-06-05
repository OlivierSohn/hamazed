{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Imj.Game.Command
      ( runClientCommand
      -- * utilities
      , withAnim
      , withAnim'
      , withGameInfoAnimationIf
      ) where

import           Imj.Prelude

import           Control.Monad.State.Strict(gets)
import           Data.Map.Strict(Map)
import qualified Data.Map.Strict as Map

import           Imj.ClientView.Types
import           Imj.Game.Class
import           Imj.Graphics.Class.DiscreteDistance
import           Imj.Graphics.Class.Words
import           Imj.Graphics.Color.Types
import           Imj.Graphics.Text.ColoredGlyphList hiding (colored)
import           Imj.Network
import           Imj.Server.Class

import           Imj.Game.Draw
import           Imj.Game.Infos
import           Imj.Game.Modify
import           Imj.Game.Show
import           Imj.Graphics.Text.ColorString
import           Imj.Graphics.Screen
import           Imj.Graphics.UI.Animation
import           Imj.Graphics.UI.Chat
import           Imj.Graphics.UI.Colored
import           Imj.Graphics.UI.RectContainer
import           Imj.Timing

runClientCommand :: (GameLogicT e ~ g
                   , MonadState (AppState (GameLogicT e)) m
                   , MonadReader e m, Client e
                   , MonadIO m)
                 => ClientId
                 -> ClientCommand (CustomCmdT (ServerT g)) Approved
                 -> m ()
runClientCommand sid cmd = getPlayer sid >>= \p -> do
  let name = getPlayerUIName' p
  case cmd of
    CustomCmd x -> onClientCustomCmd x -- TODO generically distinguish commands that need an animation and chat reporting commands
    AssignName name' ->
      withAnim $
        putPlayer sid $ Player name' Present $ maybe (mkPlayerColors (rgb 3 3 3)) getPlayerColors p
    AssignColor color ->
      withAnim $
        putPlayer sid $ Player (maybe (ClientName "") getPlayerName p) Present $ mkPlayerColors color
    Says what ->
      stateChat $ addMessage $ ChatMessage $
        name <> colored (":" <> what) chatMsgColor
    Leaves detail -> do
      maybe
        (return ())
        (\n -> withAnim $ putPlayer sid $ n { getClientStatus = Absent })
        p
      stateChat $ addMessage $ ChatMessage $
        name <>
        colored
          ((<>) " leaves the game " $
          either
            ((<>) "due to a connection error : ")
            (const "intentionally.")
            detail)
          chatMsgColor

-- | Uses 'withAnim' when a condition is met.
withGameInfoAnimationIf :: (GameLogicT e ~ g
                          , MonadState (AppState (GameLogicT e)) m
                          , MonadReader e m, Client e
                          , MonadIO m)
                        => Bool
                        -- ^ The condition
                        -> m a
                        -- ^ The action, which will be just executed or executed
                        -- inside withAnim
                        -> m a
withGameInfoAnimationIf condition act =
  f act
 where
  f = bool id withAnim condition

-- | Runs an action that may change the result of one of 'getClientsInfos', 'getViewport' or 'mkWorldInfos'
-- and schedules an animation 'UIAnimation' that will make the changes appear progressively.
withAnim :: (GameLogicT e ~ g
           , MonadState (AppState (GameLogicT e)) m
           , MonadReader e m, Client e
           , MonadIO m)
          => m a
          -- ^ The action to run.
          -> m a
withAnim = withAnim' Normal

-- | Runs an action that may change the result of one of 'getClientsInfos', 'getViewport' or 'mkWorldInfos'
-- and schedules an animation 'UIAnimation' that will make the changes appear progressively.
withAnim' :: (GameLogicT e ~ g
            , MonadState (AppState (GameLogicT e)) m
            , MonadReader e m, Client e
            , MonadIO m)
          => InfoType
          -> m a
          -> m a
withAnim' infoType act = do
  gets game >>= \(Game _ _ (GameState g1 _) _ _ names1 _ _ _ _) -> do
    res <- act
    gets game >>= \(Game _ screen (GameState g2 _) _ _ names2 _ _ _ _) -> do
      t <- liftIO getSystemTime
      putAnimation $ mkAnim infoType t screen names1 names2 g1 g2
    return res

mkAnim :: (GameExternalUI g1, GameExternalUI g2)
       => InfoType
       -> Time Point System
       -> Screen
       -> Map ClientId (Player g1)
       -- ^ from
       -> Map ClientId (Player g2)
       -- ^ to
       -> Maybe g1
       -- ^ from
       -> Maybe g2
       -- ^ to
       -> UIAnimation
mkAnim it t screen@(Screen _ center) namesI namesF gI gF =
  let (hDist, vDist) = computeViewDistances
      colorFrom = getFrameColor gI
      colorTo   = getFrameColor gF
      from = maybe mkEmptyInfos (mkWorldInfos Normal From) gI
      to   = maybe mkEmptyInfos (mkWorldInfos it     To  ) gF
      lI = maybe [] (\g -> mkClientsInfos (getClientsInfos From g) namesI) gI
      lF = maybe [] (\g -> mkClientsInfos (getClientsInfos To   g) namesF) gF

      from' = mergeInfos from lI
      to'   = mergeInfos to   lF

      rectFrom = Colored colorFrom $ maybe defaultRect (getViewport From screen) gI
      rectTo   = Colored colorTo   $ maybe defaultRect (getViewport To   screen) gF

      defaultRect = mkCenteredRectContainer center defaultFrameSize
  in mkUIAnimation (rectFrom,from') (rectTo,to') hDist vDist t

 where

  mkClientsInfos :: LeftInfo a
                 => Map ClientId a
                 -> Map ClientId (Player g)
                 -> [Successive ColoredGlyphList]
  mkClientsInfos clients players =
    case Map.elems $ safeZipMerge clients players of
      [] -> [Successive [colorize (onBlack $ gray 10) $ fromString ""]] -- for initial state when we were not given an id yet
      l -> map (\(client,player) ->
        let name = getPlayerUIName'' player
        in Successive [name <> maybe "" leftInfo client]) l

  mergeInfos :: Infos
             -> [Successive ColoredGlyphList]
             -> ((Successive ColoredGlyphList, Successive ColoredGlyphList), [Successive ColoredGlyphList])
  mergeInfos (Infos u d lu ld) l = ((u,d), lu ++ l ++ ld )
