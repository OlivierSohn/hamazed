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

import           Imj.Game.Hamazed.Level.Types

import           Imj.Game.Hamazed.Chat
import           Imj.Game.Hamazed.Color
import           Imj.Game.Hamazed.World.Types
import           Imj.Graphics.Text.ColorString

data InfoType = Normal | ColorAnimated

mkLevelCS :: InfoType -> Int -> Successive ColorString
mkLevelCS t level =
  let txt c = colored "Level " configFgColor <> colored (pack (show level)) c <> colored (" of " <> pack (show lastLevel)) configFgColor
  in Successive $ case t of
    Normal -> [txt configFgColor]
    ColorAnimated -> [txt red, txt configFgColor]

mkShipCS :: InfoType -> BattleShip -> Successive ColorString
mkShipCS _ (BattleShip (PlayerName name) _ ammo status _) =
  let pad = initialLaserAmmo - ammo
      shipNameColor Destroyed = darkConfigFgColor
      shipNameColor _   = configFgColor
      ammoColor' Destroyed = darkConfigFgColor
      ammoColor' _   = ammoColor

      s = colored ("\"" <> name <> "\"   " <> pack (replicate pad ' ')) (shipNameColor status)
       <> colored (singleton '[') bracketsColor
       <> colored (pack $ replicate ammo '.') (ammoColor' status)
       <> colored (singleton ']') bracketsColor
   in Successive [s]

mkObjectiveCS :: InfoType -> Int -> Successive ColorString
mkObjectiveCS t target =
  let txt c = colored "Objective : " configFgColor <> colored (pack (show target)) c
  in Successive $ case t of
    Normal -> [txt white]
    ColorAnimated -> [txt red, txt white]


mkShotNumbersCS :: InfoType -> [Int] -> Successive ColorString
mkShotNumbersCS _ nums =
  let lastIndex = length nums - 1
      first = colored (singleton '[') bracketsColor
      last_ = colored (singleton ']') bracketsColor
      middle = snd $ foldl' (\(i,s) n -> let num = intToDigit n
                                             t = case i of
                                                  0 -> singleton num
                                                  _ -> pack [num, ' ']
                                         in (i-1, s <> colored' t (numberColor n))) (lastIndex, first) nums

  in Successive [middle <> last_]

mkLeftInfo :: InfoType -> [BattleShip] -> [Int] -> LevelSpec -> [Successive ColorString]
mkLeftInfo t ships shotNums (LevelSpec level target _)=
  [ mkObjectiveCS t target
  , mkShotNumbersCS t shotNums
  ]
  ++
  map (mkShipCS t) ships
  ++
  [ mkLevelCS t level
  ]

mkUpDownInfo :: (Successive ColorString, Successive ColorString)
mkUpDownInfo =
  (Successive [],Successive [])

mkInfos :: InfoType
        -> [BattleShip]
        -> [Int]
        -> LevelSpec
        -> ((Successive ColorString, Successive ColorString), [Successive ColorString])
mkInfos t ships shotNums level =
  (mkUpDownInfo, mkLeftInfo t ships shotNums level)
