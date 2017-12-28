{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}

module Imj.Input.NonBlocking
    ( -- * Non-blocking read
      tryGetKeyThenFlush
    ) where

import           Imj.Prelude

import           System.IO( hReady
                          , stdin)

import           Imj.Input.Types
import           Imj.Input.Blocking

callIf :: IO a -> IO Bool -> IO (Maybe a)
callIf call condition =
  condition >>= \case
    True  -> Just <$> call
    False -> return Nothing

-- | Tries to read a key from stdin. If it succeeds, it flushes stdin.
tryGetKeyThenFlush :: IO (Maybe Key)
tryGetKeyThenFlush = getKeyThenFlush `callIf` someInputIsAvailable
  where
    someInputIsAvailable = hReady stdin
