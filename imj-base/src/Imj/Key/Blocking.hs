{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}

module Imj.Key.Blocking
    ( -- * Blocking read
      getKeyThenFlush
    ) where


import           Imj.Prelude

import           System.IO( getChar, hReady, stdin )

import           Data.Char( ord )
import           Data.List( reverse )

import           Imj.Geo.Discrete.Types( Direction(..) )
import           Imj.Key.Types

-- | Blocks until a key is read from stdin. Then, flushes stdin.
getKeyThenFlush :: IO Key
getKeyThenFlush = do
  chars <- getAllChars
  let res = fromString chars
  -- uncomment to see escape codes
  -- when (ord (head chars) == 27) $ putStrLn $ tail chars
  return res

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

-- | returns when stdin is empty
getAllChars :: IO String
getAllChars =
  reverse <$> getKey' ""
 where getKey' chars = do
         char <- getChar
         more <- hReady stdin
         (if more
            then
              getKey'
            else
              return) (char:chars)
