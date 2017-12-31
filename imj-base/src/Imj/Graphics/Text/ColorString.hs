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
import qualified Data.List as List(length)

import           Imj.Graphics.Class.DiscreteInterpolation
import           Imj.Graphics.Color.Types
import           Imj.Graphics.Text.ColorString.Interpolation
import           Imj.Util

newtype ColorString = ColorString [(Text, LayeredColor)] deriving(Show)

instance IsString ColorString where
  fromString str = ColorString [(pack str, onBlack white)]


-- TODO maybe it would be faster to have a representation with Array (Char, LayeredColor)
--  (ie the result of simplify)
-- | First interpolating characters, then color.
instance DiscreteDistance ColorString where
  distance c1 c2 =
    let colorDist (_, color) (_, color') = distance color color'
        n1 = countChars c1
        n2 = countChars c2
        s1 = simplify c1
        s2 = simplify c2

        (c1', remaining) = interpolateChars s1 s2 countTextChanges
        s1' = assert (remaining == 0) c1'
        l = zipWith colorDist s1' s2 -- since color interpolation happends AFTER char changes,
                                     -- we compare colors with result of char interpolation
        colorDistance =
          if null l
            then
              1
            else
              maximum l

        toString = map fst
        str1 = toString s1
        str2 = toString s2
        lPref = List.length $ commonPrefix str1 str2
        lSuff = List.length $ commonSuffix (drop lPref str1) (drop lPref str2)
        countTextChanges = max n1 n2 - (lPref + lSuff)
    in colorDistance + countTextChanges

-- | First interpolating characters, then color.
instance DiscreteInterpolation ColorString where
  interpolate c1 c2 i =
    let c2' = simplify c2
        (c1', remaining) = interpolateChars (simplify c1) c2' i
    in ColorString $ map (\(char,color) -> (pack [char], color)) $
        if remaining >= 0
          then
            c1'
          else
            interpolateColors c1' c2' (negate remaining)


interpolateColors :: [(Char, LayeredColor)]
                  -- ^ from
                  ->[(Char, LayeredColor)]
                  -- ^ to
                  -> Int
                  -- ^ progress
                  -> [(Char, LayeredColor)]
interpolateColors c1 c2 i =
  let z (_, color) (char, color') = (char, interpolate color color' i)
  in  zipWith z c1 c2


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
