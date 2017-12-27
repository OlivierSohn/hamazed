{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Imj.Game.Render(
        mkInfos
      , mkLeftInfo
      , mkLevelCS
      , mkAmmoCS
      , mkObjectiveCS
      , mkShotNumbersCS
      , InfoType(..)
      ) where

import           Imj.Prelude

import           Data.Char( intToDigit )
import           Data.List( length, foldl' )
import           Data.Text(pack, singleton)

import           Imj.Game.Color
import           Imj.Game.Types
import           Imj.Game.Level.Types
import           Imj.Text.ColorString

mkLevelCS :: InfoType -> Int -> [ColorString]
mkLevelCS t level =
  let txt c = colored "Level " white <> colored (pack (show level)) c <> colored (" of " <> pack (show lastLevel)) white
  in case t of
    Normal -> [txt white]
    ColorAnimated -> [txt red, txt white]

mkAmmoCS :: InfoType -> Int -> [ColorString]
mkAmmoCS _ ammo =
  let s = colored (singleton '[') bracketsColor
       <> colored (pack $ replicate ammo '.') ammoColor
       <> colored (singleton ']') bracketsColor
   in [s]

mkObjectiveCS :: InfoType -> Int -> [ColorString]
mkObjectiveCS t target =
  let txt c = colored "Objective : " white <> colored (pack (show target)) c
  in case t of
    Normal -> [txt white]
    ColorAnimated -> [txt red, txt white]


mkShotNumbersCS :: InfoType -> [Int] -> [ColorString]
mkShotNumbersCS _ nums =
  let lastIndex = length nums - 1
      first = colored (singleton '[') bracketsColor
      last_ = colored (singleton ']') bracketsColor
      middle = snd $ foldl' (\(i,s) n -> let num = intToDigit n
                                             t = case i of
                                                  0 -> singleton num
                                                  _ -> pack [num, ' ']
                                         in (i-1, s <> colored' t (numberColor n))) (lastIndex, first) nums

  in [middle <> last_]

mkLeftInfo :: InfoType -> Int -> [Int] -> [[ColorString]]
mkLeftInfo t ammo shotNums =
  [mkAmmoCS t ammo, mkShotNumbersCS t shotNums]

mkUpDownInfo :: InfoType -> Level -> ([ColorString], [ColorString])
mkUpDownInfo t (Level level target _) =
  (mkObjectiveCS t target, mkLevelCS t level)

data InfoType = Normal | ColorAnimated

mkInfos :: InfoType -> Int -> [Int] -> Level -> (([ColorString], [ColorString]), [[ColorString]])
mkInfos t ammo shotNums level =
  (mkUpDownInfo t level, mkLeftInfo t ammo shotNums)
