{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}

module Imj.IO.NonBlocking
    ( -- * Non-blocking Input
      tryGetCharThenFlush
    ) where

import           Imj.Prelude

import           System.IO( hReady
                          , stdin)

import           Imj.IO.Types
import           Imj.IO.Blocking

callIf :: IO a -> IO Bool -> IO (Maybe a)
callIf call condition =
  condition >>= \case
    True  -> Just <$> call
    False -> return Nothing

-- | Tries to read a key from stdin. If it succeeds, it flushes stdin.
tryGetCharThenFlush :: IO (Maybe Key)
tryGetCharThenFlush = getCharThenFlush `callIf` someInputIsAvailable
  where
    someInputIsAvailable = hReady stdin
