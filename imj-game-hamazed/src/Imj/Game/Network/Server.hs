{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Imj.Game.Network.Server
      ( srvColorSchemeArg
      , ColorScheme(..)
      , mkCenterColor
      , SuggestedPlayerName(..)
      , mkClientColorFromCenter
      ) where

import           Imj.Prelude

import           Data.Char(toLower)
import           Data.String(IsString(..))
import           Options.Applicative(str, ReadM, readerError)
import           Text.Read(readMaybe)

import           Imj.Graphics.Color
import           Imj.Graphics.Color.Types
import           Imj.Game.Color
import           Imj.Timing
import           Imj.ClientView.Types

newtype SuggestedPlayerName = SuggestedPlayerName String
  deriving(Generic, Eq, Show, Binary, IsString)

data ColorScheme =
    UseServerStartTime
  | ColorScheme {-# UNPACK #-} !(Color8 Foreground)
  deriving(Generic, Show)
instance NFData ColorScheme

mkCenterColor :: ColorScheme -> IO (Color8 Foreground)
mkCenterColor (ColorScheme c) = return c
mkCenterColor UseServerStartTime = do
  t <- getCurrentSecond
  let !ref = rgb 3 2 0
      nColors = countHuesOfSameIntensity ref
      n = t `mod` nColors
  return $ rotateHue (fromIntegral n / fromIntegral nColors) ref

srvColorSchemeArg :: ReadM ColorScheme
srvColorSchemeArg = map toLower <$> str >>= \lowercase -> do
  let err = readerError $
       "Encountered an invalid color scheme:\n\t" ++
       lowercase ++
       "\nAccepted values are:" ++
       "\n - one of " ++ descPredefinedColors ++
       "\n - 'rgb' | '\"r g b\"' where r,g,b are one of 0,1,2,3,4,5 (pure red is for example 500 / \"5 0 0\")" ++
       "\n - 'time'"
      asRGB l = case catMaybes $ map (readMaybe . (:[])) l of
        [r,g,b] -> either (const err) (return . ColorScheme) $ userRgb r g b
        _ -> err
  maybe
    (case lowercase of
      "time" -> return UseServerStartTime
      l@[_ ,_ ,_]     -> asRGB l
      [r,' ',g,' ',b] -> asRGB [r,g,b]
      _ -> err)
    (return . ColorScheme)
    $ predefinedColor lowercase

-- | This function assumes that ClientId's start at 0 and are ascending.
mkClientColorFromCenter :: ClientId -> Color8 Foreground -> Color8 Foreground
mkClientColorFromCenter i ref =
  let nColors = countHuesOfSameIntensity ref
      -- we want the following mapping:
      -- 0 -> 0
      -- 1 -> 1
      -- 2 -> -1
      -- 3 -> 2
      -- 4 -> -2
      -- ...
      dist = quot (succ i) 2
      n' = fromIntegral dist `mod` nColors
      n = if odd i then n' else -n'
  in rotateHue (fromIntegral n / fromIntegral nColors) ref
