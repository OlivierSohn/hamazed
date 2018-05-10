{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Imj.Game.KeysMaps
    ( translatePlatformEvent
    ) where

import           Imj.Prelude

import           Data.Proxy(Proxy(..))
import           Control.Monad.Reader.Class(MonadReader)
import qualified Graphics.UI.GLFW as GLFW

import           Imj.Event
import           Imj.Geo.Discrete.Types
import           Imj.Graphics.UI.Chat
import           Imj.Game.Class
import           Imj.Game.Status
import           Imj.Input.Types
import           Imj.Server.Class
import           Imj.Server.Types

toStateKeys :: Proxy g -> Proxy (StatefullKeysT g)
toStateKeys _ = Proxy

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
      NotEditing -> case key of
        Arrow Up    -> return $ Just $ Evt $ CycleRenderingOptions (-1) 0
        Arrow Down  -> return $ Just $ Evt $ CycleRenderingOptions 1 0
        Arrow LEFT  -> return $ Just $ Evt $ CycleRenderingOptions 0 (-1)
        Arrow RIGHT -> return $ Just $ Evt $ CycleRenderingOptions 0 1
        -- commented out, replaced by precomputed PPU / Margins associations (see 4 lines above)
        {-
        AlphaNum 'h' -> return $ Just $ Evt $ ApplyPPUDelta $ Size 1 0
        AlphaNum 'n' -> return $ Just $ Evt $ ApplyPPUDelta $ Size (-1) 0
        AlphaNum 'b' -> return $ Just $ Evt $ ApplyPPUDelta $ Size 0 (-1)
        AlphaNum 'm' -> return $ Just $ Evt $ ApplyPPUDelta $ Size 0 1
        AlphaNum 'g' -> return $ Just $ Evt $ ApplyFontMarginDelta $ -1
        AlphaNum 'v' -> return $ Just $ Evt $ ApplyFontMarginDelta 1
        -}
        _ -> do
          (ClientState activity state) <- getClientState <$> gets game
          case activity of
            Over -> return Nothing
            Ongoing -> case state of
              Excluded -> return Nothing
              Included x -> mapInterpretedKey key x
  StatefullKey k s m -> do
    (ClientState activity state) <- getClientState <$> gets game
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
          send = mapStateKey (toStateKeys prox) k s m x
