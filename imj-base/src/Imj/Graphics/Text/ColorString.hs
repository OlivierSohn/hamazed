{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}

  {- | A 'ColorString' is a multicolored 'Text'.-}

module Imj.Graphics.Text.ColorString
            (
            -- * Type
              ColorString(..)
            -- * Constructors
{- | 'colored' creates a 'ColorString' using the specified foreground color on
/black/ background, wherease 'colored'' allows you to chose both the
background and the foreground colors.

And since 'ColorString' is 'Monoid', we can write:

@
str = colored \"Hello\" white <> colored \" World\" yellow
@
 -}
            , colored
            , colored'
            -- * Utilities
            , countChars
            , take
            -- * Reexports
            , LayeredColor(..)
            ) where

import           Imj.Prelude hiding(take)

import           Data.Char(isSpace)
import           Data.String(IsString(..))
import qualified Data.Text as Text( pack, unpack, length, take, words, last, head, cons, splitAt)
import qualified Data.List as List(length)

import           Imj.Graphics.Class.DiscreteInterpolation
import           Imj.Graphics.Class.Words
import           Imj.Graphics.Color.Types
import           Imj.Graphics.Text.ColorString.Interpolation
import           Imj.Util

newtype ColorString = ColorString [(Text, LayeredColor)] deriving(Show, Generic)

-- we can't use the Generic one because of missing instance for 'Text'
instance PrettyVal ColorString where
  prettyVal c = prettyVal $ map fst $ simplify c

instance IsString ColorString where
  fromString str = ColorString [(Text.pack str, onBlack white)]


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
    in ColorString $ map (\(char,color) -> (Text.pack [char], color)) $
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
       (Text.unpack txt)
     ++ simplify (ColorString $ tail l)


colored' :: Text -> LayeredColor -> ColorString
colored' t c = ColorString [(t, c)]

colored :: Text -> Color8 Foreground -> ColorString
colored t c = colored' t $ onBlack c

-- | Counts the chars in the 'ColorString'
countChars :: ColorString -> Int -- TODO rename as length
countChars (ColorString cs) =
  sum $ map (Text.length . fst) cs

take :: Int -> ColorString -> ColorString
take nChars (ColorString l)
  | nChars <= 0 = mempty
  | otherwise = ColorString $ reverse $ take' nChars [] l
  where
    take' :: Int -> [(Text, LayeredColor)] -> [(Text, LayeredColor)] -> [(Text, LayeredColor)]
    take' _ cur [] = cur
    take' 0 cur _  = cur
    take' n cur (r@(txt,color):rs)
      | n >= lr = take' (n-lr) (r:cur) rs
      | otherwise = (Text.take n txt, color):cur
      where lr = Text.length txt

instance Monoid ColorString where
  mempty = ColorString []
  mappend (ColorString x) (ColorString y) = ColorString $ x ++ y

instance Words ColorString where
  length = countChars

  splitAt idx (SingleWord (ColorString l)) =
    (SingleWord $ ColorString $ reverse left
   , SingleWord $ ColorString $ reverse right)
    where
      (left, right) = split' idx l ([],[])
      split' :: Int
             -> [(Text,LayeredColor)]
             -> ([(Text,LayeredColor)], [(Text,LayeredColor)])
             -> ([(Text,LayeredColor)], [(Text,LayeredColor)])
      split' _ [] cur = cur
      split' n (t@(txt,color):xs) (lefts,rights)
        | n >= len  = split' (n-len) xs (            t:lefts,                rights)
        | n > 0     = split' 0       xs ((tLeft,color):lefts, (tRight,color):rights)
        | otherwise = split' 0       xs (              lefts,              t:rights)
        where
          len = Text.length txt
          (tLeft,tRight) = Text.splitAt n txt

  unwords :: [SingleWord ColorString] -> ColorString
  unwords strs =
    unwords' (reverse strs) mempty
   where
    unwords' [] cur = cur
    unwords' (SingleWord x:xs) (ColorString []) =
      unwords' xs x
    unwords' (SingleWord (ColorString x):xs) (ColorString ((c,color):cs)) =
      unwords' xs $ ColorString $ x ++ ((Text.cons ' ' c,color):cs)

  words :: ColorString -> [SingleWord ColorString]
  words (ColorString l) =
    map (SingleWord . ColorString) $ reverse $ words' l False []
   where
    words' :: [(Text,LayeredColor)] -> Bool -> [[(Text,LayeredColor)]] -> [[(Text,LayeredColor)]]
    words' [] _ cur = cur
    words' ((txt,color):xs) prevShouldMergeLastWord cur =
      case Text.words txt of
        [] ->
          words' xs prevShouldMergeLastWord cur
        asWords@(firstWord:lastWords) ->
          words' xs (not $ isSpace $ Text.last txt) cur'
         where
          cur' =
            if not (isSpace $ Text.head txt) && prevShouldMergeLastWord
              then
                -- we merge this iteration's first word with the previous iteration's
                -- last word.
                case cur of
                  [] -> error "prevShouldMergeLastWord is True, cur cannot be empty"
                  c:cs -> map (\aw -> [(aw, color)]) (reverse lastWords)
                          ++ ((c ++ [(firstWord, color)]) : cs)
              else
                -- we don't merge words
                map (\aw -> [(aw, color)]) (reverse asWords) ++ cur
  {-# INLINABLE words #-}
  {-# INLINABLE unwords #-}
  {-# INLINABLE length #-}
  {-# INLINABLE splitAt #-}
