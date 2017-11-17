{-# LANGUAGE LambdaCase #-}

module NonBlockingIO
    ( tryGetChar
    ) where

import           Imajuscule.Prelude

import           System.IO( getChar
                          , hReady
                          , stdin)


--------------------------------------------------------------------------------
-- Pure
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- IO
--------------------------------------------------------------------------------

callIf :: IO a -> IO Bool -> IO (Maybe a)
callIf call condition =
  condition >>= \case
    True  -> (Just <$> call)
    False -> (return Nothing)

tryGetChar :: IO (Maybe Char)
tryGetChar = getChar `callIf` someInputIsAvailable
  where
    someInputIsAvailable = hReady stdin
