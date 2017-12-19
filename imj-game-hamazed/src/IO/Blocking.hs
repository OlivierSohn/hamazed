
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}

module IO.Blocking
    ( getCharThenFlush
    ) where


import           Imajuscule.Prelude
-- import           Prelude(putStrLn)

import           System.IO( getChar, hReady, stdin )

import           Data.Char( ord )
import           Data.List( reverse )
import           Data.String( String )

import           Geo.Types( Direction(..) )
import           IO.Types


getCharThenFlush :: IO (Either Key Char)
getCharThenFlush = do
  chars <- getAllChars
  let res = fromString chars
  -- uncomment to see escape codes
  -- when (ord (head chars) == 27) $ putStrLn $ tail chars
  return res

fromString :: String -> Either Key Char
fromString =
  \case
    [] -> error "should not be empty"
    [c] -> case ord c of
             27 {-ESC-} -> Left Escape
             _          -> Right c
    c:l -> case ord c of
             27 {-ESC-} -> case l of
                         a:b:_ -> case a of
                                    '[' -> case b of
                                             'A' -> Left $ Arrow Up
                                             'B' -> Left $ Arrow Down
                                             'C' -> Left $ Arrow RIGHT
                                             'D' -> Left $ Arrow LEFT
                                             _ -> Left Unknown
                                    _ -> Left Unknown
                         _ -> Left Unknown
             _ -> Right c

-- | returns when stdin is empty
getAllChars :: IO String
getAllChars = reverse <$> getKey' ""
  where getKey' chars = do
          char <- getChar
          more <- hReady stdin
          (if more then getKey' else return) (char:chars)
