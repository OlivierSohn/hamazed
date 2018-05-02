{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Imj.Game.Hamazed.KeysMaps
    ( translatePlatformEvent
    ) where

import           Imj.Prelude

import           Control.Monad.Reader.Class(MonadReader)

import           Imj.Event
import           Imj.Game.Hamazed.Loop.Event.Types
import           Imj.Game.Hamazed.State.Types
import           Imj.Game.Hamazed.Network.Types
import           Imj.Game.Hamazed.World.Space.Types
import           Imj.Game.Hamazed.Types
import           Imj.Input.Types

translatePlatformEvent :: (GameLogicT e ~ g
                         , MonadState (AppState g) m
                         , MonadReader e m, Client e)
                       => PlatformEvent
                       -> m (Maybe (GenEvent g))
translatePlatformEvent k = case k of
  Message msgLevel txt -> return $ Just $ Evt $ Log msgLevel txt
  StopProgram -> return $ Just $ CliEvt $ RequestApproval $ Leaves $ Right ()
  FramebufferSizeChanges -> return $ Just $ Evt RenderingTargetChanged
  KeyPress key -> case key of
    Escape      -> return $ Just $ CliEvt $ RequestApproval $ Leaves $ Right ()
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
        {-
        KeyPress (AlphaNum '1') -> return $ Just $ Evt $ PlayProgram 1
        KeyPress (AlphaNum '2') -> return $ Just $ Evt $ PlayProgram 2
        KeyPress (AlphaNum '3') -> return $ Just $ Evt $ PlayProgram 3
        KeyPress (AlphaNum '4') -> return $ Just $ Evt $ PlayProgram 4
        KeyPress (AlphaNum '5') -> return $ Just $ Evt $ PlayProgram 5
        KeyPress (AlphaNum '6') -> return $ Just $ Evt $ PlayProgram 6
        KeyPress (AlphaNum '7') -> return $ Just $ Evt $ PlayProgram 7
        KeyPress (AlphaNum '8') -> return $ Just $ Evt $ PlayProgram 8
        KeyPress (AlphaNum '9') -> return $ Just $ Evt $ PlayProgram 9
        KeyPress (AlphaNum '0') -> return $ Just $ Evt $ PlayProgram 0
        -}
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
            Ongoing -> keyMaps key state
