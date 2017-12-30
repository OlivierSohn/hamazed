{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Imj.Graphics.Text.ColorString.Interpolation
            ( -- * Interpolation
              interpolateChars
            ) where

import           Imj.Prelude

import           Data.List(length, splitAt)

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
      -- TODO use source color when replacing a char (color will be interpolated later on)
      cdReplaced = take nCDReplaced s2AfterCommonPref

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
      ed1 = take nExDiff1 $ drop totalCD s1AfterCommonPref
      ed2 = take nExDiff2 $ drop totalCD s2AfterCommonPref

      remaining = (totalCD + abs signedTotalExDiff) - i

  in ( commonPref ++ cdReplaced ++ cdUnchanged ++ ed1 ++ ed2 ++ commonSuff
     , assert (remaining == max n1 n2 - (lPref + lSuff) - i) remaining)
