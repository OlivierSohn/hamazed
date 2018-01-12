{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_HADDOCK hide #-}

module Imj.Graphics.Render.Delta.Backend
               ( withDeltaRendering
               , withDeltaRendering'
               , DeltaRendering(..)
               ) where

import           Imj.Prelude

import           Control.Exception( finally )

import           Imj.Graphics.Render.Delta.Backend.Types
import           Imj.Graphics.Render.Delta.Backend.Console
import           Imj.Graphics.Render.Delta.Env
import           Imj.Graphics.Color.Types
import           Imj.Threading

withDeltaRendering' :: DeltaRendering
                    -> Maybe ResizePolicy
                    -> Maybe ClearPolicy
                    -> Maybe (Color8 Background)
                    -- ^ Color to clear with
                    -> (DeltaEnv -> IO ())
                    -- ^ Action using a delta-rendering environment.
                    -> IO ()
withDeltaRendering' t p1 p2 p3 action = do
  let (Backend initialize render cleanup) = case t of
        Console -> mkConsoleBackend Nothing
        OpenGL -> undefined
  -- When Ctrl+C is hit, an exception is thrown on the main thread, hence
  -- I use 'finally' to cleanup.
  (void $ runAndWaitForTermination $ do
    initialize
    newEnv render p1 p2 p3 >>= action)
    `finally`
    cleanup

withDeltaRendering :: DeltaRendering
                   -> (DeltaEnv -> IO ())
                   -- ^ Action using a delta-rendering environment.
                   -> IO ()
withDeltaRendering t action =
  withDeltaRendering' t Nothing Nothing Nothing action
