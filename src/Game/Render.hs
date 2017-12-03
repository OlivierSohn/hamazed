{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Game.Render(
        mkInfos
      , mkLeftInfo
      , mkLevelCS
      , mkAmmoCS
      , mkObjectiveCS
      , mkShotNumbersCS
      ) where

import           Imajuscule.Prelude

import           Data.Char( intToDigit )
import           Data.List( length, foldl' )
import           Data.Text(pack, singleton)

import           Color
import           Game.Types
import           Text.ColorString

mkLevelCS :: Int -> ColorString
mkLevelCS level =
  colored ("Level " <> pack (show level) <> " of " <> pack (show lastLevel)) white

mkAmmoCS :: Int -> ColorString
mkAmmoCS ammo =
      colored (singleton '[') bracketsColor
   <> colored (pack $ replicate ammo '.') ammoColor
   <> colored (singleton ']') bracketsColor

mkObjectiveCS :: Int -> ColorString
mkObjectiveCS target =
  colored ("Objective : " <> pack (show target)) white


mkShotNumbersCS :: [Int] -> ColorString
mkShotNumbersCS nums =
  let lastIndex = length nums - 1
      first = colored (singleton '[') bracketsColor
      last_ = colored (singleton ']') bracketsColor
      middle = snd $ foldl' (\(i,s) n -> let num = intToDigit n
                                             t = case i of
                                                  0 -> singleton num
                                                  _ -> pack [num, ' ']
                                         in (i-1, s <> colored t (numberColor n))) (lastIndex, first) nums

  in middle <> last_

mkLeftInfo :: Int -> [Int] -> (ColorString, ColorString)
mkLeftInfo ammo shotNums =
  (mkAmmoCS ammo, mkShotNumbersCS shotNums)

mkUpDownInfo :: Level -> (ColorString, ColorString)
mkUpDownInfo (Level level target _) =
  (mkObjectiveCS target, mkLevelCS level)

mkInfos :: Int -> [Int] -> Level -> ((ColorString, ColorString), (ColorString, ColorString))
mkInfos ammo shotNums level =
  (mkUpDownInfo level, mkLeftInfo ammo shotNums)
