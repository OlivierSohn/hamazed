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
import           Color.Interpolation

import           Data.String(IsString(..))
import           Data.Text( Text, length, pack, unpack )

newtype ColorString = ColorString [(Text, Color8Code)]

simplify :: ColorString -> [(Char, Color8Code)]
simplify (ColorString []) = []
simplify (ColorString l@(_:_)) =
  let (txt, color) = head l
  in map (\c -> (c,color)) (unpack txt) ++ simplify (ColorString $ tail l)

-- for now, we support only the color animations (no change of character or length)
instance DiscretelyInterpolable ColorString where
  distance c1 c2 =
    let colorDist (_, color) (_, color') = bresenhamColor8Length color color'
        l = zipWith colorDist (simplify c1) (simplify c2)
    in succ $ if null l
                then
                  0
                else
                  maximum l

  interpolate c1 c2 i =
    let itp (_, color) (char, color') =
          (pack [char],
          let (IColor8Code res) = interpolate (IColor8Code color) (IColor8Code color') i in res)
    in ColorString $ zipWith itp (simplify c1) (simplify c2)

instance IsString ColorString where
  fromString str = ColorString [(pack str, white)]

colored :: Text -> Color8Code -> ColorString
colored t c = ColorString [(t,c)]

countChars :: ColorString -> Int
countChars (ColorString cs) = sum $ map (length . fst) cs

instance Monoid ColorString where
  mempty = ColorString [("", Color8Code 0)]
  mappend (ColorString x) (ColorString y) = ColorString $ x ++ y
