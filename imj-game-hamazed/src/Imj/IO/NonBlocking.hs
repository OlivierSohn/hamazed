{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}

module Imj.IO.NonBlocking
    ( tryGetCharThenFlush
    ) where

import           Imj.Prelude

import           System.IO( hReady
                          , stdin)

import           Imj.IO.Types
import           Imj.IO.Blocking

callIf :: IO a -> IO Bool -> IO (Maybe a)
callIf call condition =
  condition >>= \case
    True  -> (Just <$> call)
    False -> (return Nothing)

tryGetCharThenFlush :: IO (Maybe (Either Key Char))
tryGetCharThenFlush = getCharThenFlush `callIf` someInputIsAvailable
  where
    someInputIsAvailable = hReady stdin
