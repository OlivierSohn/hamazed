{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Imj.Game.Hamazed.Infos(
        mkInfos
      , mkLeftInfo
      , InfoType(..)
      ) where

import           Imj.Prelude

import           Data.Char( intToDigit )
import           Data.List( length, foldl' )
import           Data.Text(pack, singleton)

import           Imj.Game.Hamazed.Color
import           Imj.Game.Hamazed.Level.Types
import           Imj.Graphics.Text.ColorString

data InfoType = Normal | ColorAnimated

mkLevelCS :: InfoType -> Int -> [ColorString]
mkLevelCS t level =
  let txt c = colored "Level " configFgColor <> colored (pack (show level)) c <> colored (" of " <> pack (show lastLevel)) configFgColor
  in case t of
    Normal -> [txt configFgColor]
    ColorAnimated -> [txt red, txt configFgColor]

mkAmmoCS :: InfoType -> Int -> [ColorString]
mkAmmoCS _ ammo =
  let s = colored (singleton '[') bracketsColor
       <> colored (pack $ replicate ammo '.') ammoColor
       <> colored (singleton ']') bracketsColor
   in [s]

mkObjectiveCS :: InfoType -> Int -> [ColorString]
mkObjectiveCS t target =
  let txt c = colored "Objective : " configFgColor <> colored (pack (show target)) c
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

mkLeftInfo :: InfoType -> Int -> [Int] -> Level -> [[ColorString]]
mkLeftInfo t ammo shotNums (Level level target _)=
  [ mkObjectiveCS t target
  , mkShotNumbersCS t shotNums
  , mkAmmoCS t ammo
  , mkLevelCS t level
  ]

mkUpDownInfo :: ([ColorString], [ColorString])
mkUpDownInfo =
  ([],[])

mkInfos :: InfoType -> Int -> [Int] -> Level -> (([ColorString], [ColorString]), [[ColorString]])
mkInfos t ammo shotNums level =
  (mkUpDownInfo, mkLeftInfo t ammo shotNums level)
