{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Imj.Game.KeysMaps
    ( translatePlatformEvent
    , shiftOnly
    , isArrow
    ) where

import           Imj.Prelude

import           Data.Proxy(Proxy(..))
import           Control.Monad.State.Strict(gets)
import           Control.Monad.Reader.Class(MonadReader)
import qualified Graphics.UI.GLFW as GLFW

import           Imj.Event
import           Imj.Geo.Discrete.Types
import           Imj.Graphics.UI.Chat
import           Imj.Game.Class
import           Imj.Game.Modify
import           Imj.Game.Status
import           Imj.Input.Types
import           Imj.Server.Class
import           Imj.Server.Types

toStateKeys :: Proxy g -> Proxy (StatefullKeysT g)
toStateKeys _ = Proxy

shiftOnly :: GLFW.ModifierKeys
shiftOnly = GLFW.ModifierKeys {
    GLFW.modifierKeysShift   = True
  , GLFW.modifierKeysControl = False
  , GLFW.modifierKeysAlt     = False
  , GLFW.modifierKeysSuper   = False
  }

translatePlatformEvent :: (GameLogicT e ~ g
                         , MonadState (AppState g) m
                         , MonadReader e m, Client e)
                       => Proxy g -> PlatformEvent -> m (Maybe (GenEvent g))
translatePlatformEvent prox = \case
  Message msgLevel txt -> return $ Just $ Evt $ Log msgLevel txt
  StopProgram -> return $ Just $ CliEvt $ OnCommand $ RequestApproval $ Leaves $ Right ()
  FramebufferSizeChanges -> return $ Just $ Evt RenderingTargetChanged
  InterpretedKey key -> case key of
    Escape -> return $ Just $ CliEvt $ OnCommand $ RequestApproval $ Leaves $ Right ()
    Tab -> return $ Just $ Evt $ ChatCmd ToggleEditing
    _ -> getChatMode >>= \case
      Editing -> return $ case key of
        Delete     -> Just $ Evt $ ChatCmd DeleteAtEditingPosition
        BackSpace  -> Just $ Evt $ ChatCmd DeleteBeforeEditingPosition
        AlphaNum c -> Just $ Evt $ ChatCmd $ Insert c
        Arrow dir  -> Just $ Evt $ ChatCmd $ Navigate dir
        Enter      -> Just $ Evt $ SendChatMessage
        _ -> Nothing
      NotEditing -> do
        g <- gets game
        let (ClientState activity state) = getClientState g
        case activity of
          Over -> return Nothing
          Ongoing -> case state of
            Excluded -> return Nothing
            Included x -> mapInterpretedKey key x g
  StatefullKey k s@GLFW.KeyState'Pressed m ->
    maybe
      (statefull k s m)
      (\dir ->
        -- I first filtered on 'Control' but it would work only for 'Arrow Up' on OSX
        -- (or maybe my Macbook air keyboard has a bug?), hence I now filter on 'Shift' instead.
        if m == shiftOnly
          then
            return $ Just $ Evt $
              case dir of
                Up    -> CycleRenderingOptions (-1) 0
                Down  -> CycleRenderingOptions 1 0
                LEFT  -> CycleRenderingOptions 0 (-1)
                RIGHT -> CycleRenderingOptions 0 1
                -- replaced by precomputed PPU / Margins associations (see 4 lines above)
                {-
                AlphaNum 'h' -> return $ Just $ Evt $ ApplyPPUDelta $ Size 1 0
                AlphaNum 'n' -> return $ Just $ Evt $ ApplyPPUDelta $ Size (-1) 0
                AlphaNum 'b' -> return $ Just $ Evt $ ApplyPPUDelta $ Size 0 (-1)
                AlphaNum 'm' -> return $ Just $ Evt $ ApplyPPUDelta $ Size 0 1
                AlphaNum 'g' -> return $ Just $ Evt $ ApplyFontMarginDelta $ -1
                AlphaNum 'v' -> return $ Just $ Evt $ ApplyFontMarginDelta 1
                -}
          else
            statefull k s m)
      $ isArrow k
  StatefullKey k s m ->
    statefull k s m
 where
  statefull k s m = do
    g <- gets game
    let (ClientState activity state) = getClientState g
    case activity of
      Over -> return Nothing
      Ongoing -> case state of
        Excluded -> return Nothing
        Included x -> case s of
          GLFW.KeyState'Released -> -- Let released keys through if a matching pressed key was passed
                                    -- to the handling function.
            takeKeyPressed k >>= bool (return Nothing) send -- I'm not sure if modifiers should be taken into account for matching?
          _ -> getChatMode >>= \case
            Editing -> return Nothing
            NotEditing -> do
              case s of
                GLFW.KeyState'Pressed -> addKeyPressed k
                _ -> return ()
              send
         where
          send = mapStateKey (toStateKeys prox) k s m x g

isArrow :: GLFW.Key -> Maybe Direction
isArrow = \case
  GLFW.Key'Left -> Just LEFT
  GLFW.Key'Right -> Just RIGHT
  GLFW.Key'Up -> Just Up
  GLFW.Key'Down -> Just Down
  _ -> Nothing
