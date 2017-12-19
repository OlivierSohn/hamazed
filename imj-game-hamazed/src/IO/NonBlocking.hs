{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}

module IO.NonBlocking
    ( tryGetCharThenFlush
    ) where

import           Imajuscule.Prelude

import           System.IO( hReady
                          , stdin)

import           IO.Types
import           IO.Blocking

callIf :: IO a -> IO Bool -> IO (Maybe a)
callIf call condition =
  condition >>= \case
    True  -> (Just <$> call)
    False -> (return Nothing)

tryGetCharThenFlush :: IO (Maybe (Either Key Char))
tryGetCharThenFlush = getCharThenFlush `callIf` someInputIsAvailable
  where
    someInputIsAvailable = hReady stdin
