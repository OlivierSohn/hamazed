{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Imj.Graphics.Text.ColorString.Interpolation
            ( -- * Interpolation
              interpolateChars
              -- * Helpers
            , insertionColors
            ) where

import           Imj.Prelude

import           Data.List(length, splitAt)

import           Imj.Graphics.Class.DiscreteInterpolation
import           Imj.Graphics.Color.Types
import           Imj.Util


interpolateChars :: [(Char, LayeredColor)]
                 -- ^ from
                 ->[(Char, LayeredColor)]
                 -- ^ to
                 -> Int
                 -- ^ progress
                 -> ([(Char, LayeredColor)], Int)
                 -- ^ (result,nSteps)
                 --             | >=0 : "remaining until completion"
                 --             | <0  : "completed since" (using abolute value))
interpolateChars s1 s2 i =
  let n1 = length s1
      n2 = length s2

      toString = map fst
      str1 = toString s1
      str2 = toString s2
      lPref = length $ commonPrefix str1 str2
      lSuff = length $ commonSuffix (drop lPref str1) (drop lPref str2)

      -- common prefix, common suffix

      (commonPref, s1AfterCommonPref) = splitAt lPref s1
      commonSuff = drop (n1 - (lSuff + lPref)) s1AfterCommonPref

      -- common differences (ie char changes)

      totalCD = min n1 n2 - (lPref + lSuff)
      nCDReplaced = clamp i 0 totalCD

      s2AfterCommonPref = drop lPref s2
      cdReplaced =
        -- start with the color of the old char to have a smooth color transition:
        zipWith
          (\(_, color1) (char2, _) -> (char2, color1))
          (take nCDReplaced s1AfterCommonPref)
          (take nCDReplaced s2AfterCommonPref)

      nCDUnchanged = totalCD - nCDReplaced
      cdUnchanged = take nCDUnchanged $ drop nCDReplaced s1AfterCommonPref

      -- exclusive differences (ie char deletion or insertion)
      -- TODO if n1 > n2, reduce before replacing
      signedTotalExDiff = n2 - n1
      signedNExDiff = signum signedTotalExDiff * clamp (i - totalCD) 0 (abs signedTotalExDiff)
      (nExDiff1,nExDiff2) =
        if signedTotalExDiff >= 0
          then
            (0, signedNExDiff)
          else
            (abs $ signedTotalExDiff - signedNExDiff, 0)
      -- TODO use an already existing color instead of switching to the new color immediately
      ed1 = take nExDiff1 $ drop totalCD s1AfterCommonPref
      ed2 = zipWith
              (\idx (char, color) -> (char, fromMaybe color $ insertionColors insertionBounds idx nExDiff2))
              [0..]
              $ take nExDiff2 $ drop totalCD s2AfterCommonPref

      insertionBounds :: [LayeredColor]
      insertionBounds = catMaybes $
        [ if null pre
            then
              Nothing
            else
              Just $ snd $ last pre
        , if null commonSuff
            then
              Nothing
            else
              Just $ snd $ head commonSuff ]

      remaining = (totalCD + abs signedTotalExDiff) - i

      pre = commonPref ++ cdReplaced ++ cdUnchanged
  in ( pre ++ ed1 ++ ed2 ++ commonSuff
     , assert (remaining == max n1 n2 - (lPref + lSuff) - i) remaining)

-- | Computes color to be applied when a character is inserted
-- in a 'ColorString' (during inteprolation) so that color matches right and or left
-- colors.
insertionColors :: [LayeredColor] -> Int -> Int -> Maybe LayeredColor
insertionColors insertionBounds n total =
  case insertionBounds of
    [] -> Nothing
    [color] -> Just color
    [colorFrom, colorTo] ->
      let dist = distance colorFrom colorTo
          -- when n == -1    we are at colorFrom (frame = 0)
          -- when n == total we are at colorTo   (frame = pred dist)
          frame = round (fromIntegral ((n+1) * pred dist) / fromIntegral (total+1) :: Float)
      in Just $ interpolate colorFrom colorTo frame
    _ -> error "insertionBounds has at most 2 elements"
