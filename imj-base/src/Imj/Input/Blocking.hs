{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}

module Imj.Input.Blocking
    ( -- * Blocking read
      timeoutGetKeyThenFlush
    , getKeyThenFlush
    ) where


import           Imj.Prelude

import           System.IO( getChar, hReady, stdin )
import           System.Timeout( timeout )

import           Data.Char( ord )
import           Data.List( reverse )

import           Imj.Geo.Discrete.Types( Direction(..) )
import           Imj.Input.Types


-- | Blocks until a key is read from stdin. Then, flushes stdin.
getKeyThenFlush :: IO Key
getKeyThenFlush =
  -- 'timeout' interprets strictly negative values as infinite time.
  fromMaybe (error "logic") <$> timeoutGetKeyThenFlush (-1)

-- | Like 'getKeyThenFlush' but with a timeout /for the first character read/.
-- After the first character read, we keep reading characters while stdin contains some.
-- This way, we won't interrupt reads of escaped characters.
timeoutGetKeyThenFlush :: Int -> IO (Maybe Key)
timeoutGetKeyThenFlush dt =
  fmap fromString <$> timeoutGetAllChars dt

fromString :: String -> Key
fromString =
  \case
    [] -> error "should not be empty"
    [c] -> case ord c of
             27 {-ESC-} -> Escape
             _          -> AlphaNum c
    c:l -> case ord c of
             27 {-ESC-} -> case l of
                         a:b:_ -> case a of
                                    '[' -> case b of
                                             'A' -> Arrow Up
                                             'B' -> Arrow Down
                                             'C' -> Arrow RIGHT
                                             'D' -> Arrow LEFT
                                             _ -> Unknown
                                    _ -> Unknown
                         _ -> Unknown
             _ -> AlphaNum c

-- | returns when stdin is empty, or when the timeout was hit for the first character read.
timeoutGetAllChars :: Int -> IO (Maybe String)
timeoutGetAllChars dt =
  fmap reverse <$> getKey' ""
 where
  getKey' chars =
    -- timeout only applies to the first character read:
    (if null chars
      then
        timeout dt getChar
      else
        Just <$> getChar)
      >>= maybe
        (return Nothing)
        (\char -> hReady stdin >>= \more ->
          (if more
             then
               getKey'
             else
               return . Just) (char:chars))
