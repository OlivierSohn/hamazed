{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Imj.Graphics.Text.ColorString
            (
            -- * Type
            {- | A 'ColorString' is a multicolored 'Text'.-}
              ColorString(..)
            -- * Constructors
            {- | 'colored' creates a 'ColorString' using the specified foreground color on
            /black/ background, wherease 'colored'' allows you to chose both the
            background and the foreground colors. -}
            , colored
            , colored'
            -- * Utilities
            , countChars
            , simplify
            -- * Reexports
            , LayeredColor(..)
            ) where

import           Imj.Prelude

import           Data.String(IsString(..))
import           Data.Text( Text, pack, unpack, length )

import           Imj.Graphics.Color.Types

newtype ColorString = ColorString [(Text, LayeredColor)] deriving(Show)

instance IsString ColorString where
  fromString str = ColorString [(pack str, onBlack white)]

-- | Maps a 'ColorString' to a list of 'Char' and 'LayeredColor'.
-- It is used to simplify the implementation of some interpolation algorithms
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

-- | Counts the chars in the 'ColorString'
countChars :: ColorString -> Int
countChars (ColorString cs) = sum $ map (Data.Text.length . fst) cs

instance Monoid ColorString where
  mempty = ColorString [("", onBlack white)]
  mappend (ColorString x) (ColorString y) = ColorString $ x ++ y
