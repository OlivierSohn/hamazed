{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.ColorString
            ( ColorString(..)
            , colored
            , countChars
            ) where

import           Imajuscule.Prelude

import           System.Console.ANSI(Color8Code(..))

import           Data.List(foldl')
import           Data.Text( Text, length )

newtype ColorString = ColorString [(Text, Color8Code)]

colored :: Text -> Color8Code -> ColorString
colored t c = ColorString [(t,c)]

countChars :: ColorString -> Int
countChars (ColorString cs) = foldl' (\c (txt, _) -> c + length txt) 0 cs

instance Monoid ColorString where
  mempty = ColorString [("", Color8Code 0)]
  mappend (ColorString x) (ColorString y) = ColorString $ x ++ y
