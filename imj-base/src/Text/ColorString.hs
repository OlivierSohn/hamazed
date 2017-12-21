{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.ColorString
            (
            -- * Type
              ColorString(..)
            -- * Constructors
            , colored
            , colored'
            -- * Utilities
            , countChars
            , simplify
            -- * Reexports
            , LayeredColor(..)
            ) where

import           Imajuscule.Prelude

import           Data.String(IsString(..))
import           Data.Text( Text, length, pack, unpack )

import           Color

newtype ColorString = ColorString [(Text, LayeredColor)] deriving(Show)

instance IsString ColorString where
  fromString str = ColorString [(pack str, onBlack white)]


simplify :: ColorString -> [(Char, LayeredColor)]
simplify (ColorString []) = []
simplify (ColorString l@(_:_)) =
  let (txt, color) = head l
  in map
       (\c -> (c,color))
       (unpack txt)
     ++ simplify (ColorString $ tail l)


colored' :: Text -> LayeredColor -> ColorString
colored' t c = ColorString [(t, c)]

colored :: Text -> Color8 Foreground -> ColorString
colored t c = colored' t $ onBlack c

countChars :: ColorString -> Int
countChars (ColorString cs) = sum $ map (length . fst) cs

instance Monoid ColorString where
  mempty = ColorString [("", onBlack white)]
  mappend (ColorString x) (ColorString y) = ColorString $ x ++ y
