{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.ColorString
            ( ColorString(..)
            , colored
            , countChars
            ) where

import           Imajuscule.Prelude

import           System.Console.ANSI(Color8Code(..))

import           Color

import           Data.String(IsString(..))
import           Data.Text( Text, length, pack )

newtype ColorString = ColorString [(Text, Color8Code)]

--TODO instance DiscretelyInterpolable ColorString where

instance IsString ColorString where
  fromString str = ColorString [(pack str, white)]

colored :: Text -> Color8Code -> ColorString
colored t c = ColorString [(t,c)]

countChars :: ColorString -> Int
countChars (ColorString cs) = sum $ map (length . fst) cs

instance Monoid ColorString where
  mempty = ColorString [("", Color8Code 0)]
  mappend (ColorString x) (ColorString y) = ColorString $ x ++ y
