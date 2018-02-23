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
import           Data.Map.Strict((!?))
import           Data.Text(pack, singleton)

import           Imj.Game.Hamazed.Network.Types
import           Imj.Game.Hamazed.Types

import           Imj.Game.Hamazed.Color
import           Imj.Graphics.Text.ColorString

data InfoType = Normal | ColorAnimated

mkLevelCS :: InfoType -> Int -> Successive ColorString
mkLevelCS t level =
  let txt c = colored "Level " configFgColor <> colored (pack (show level)) c <> colored (" of " <> pack (show lastLevel)) configFgColor
  in Successive $ case t of
    Normal -> [txt configFgColor]
    ColorAnimated -> [txt red, txt configFgColor]

mkShipCS :: InfoType
         -> Map ShipId Player -- TODO use the AppState monad instead of passing this ?
         -> BattleShip
         -> Successive ColorString
mkShipCS _ names (BattleShip sid _ ammo status _) =
  let name = getPlayerUIName $ names !? sid
      pad = initialLaserAmmo - ammo
      shipNameColor Destroyed = darkConfigFgColor
      shipNameColor _   = configFgColor
      ammoColor' Destroyed = darkConfigFgColor
      ammoColor' _   = ammoColor
      c = shipNameColor status

      s = name
       <> colored ("   " <> pack (replicate pad ' ')) c
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


mkShotNumbersCS :: InfoType -> [ShotNumber] -> Successive ColorString
mkShotNumbersCS _ nums =
  let lastIndex = length nums - 1
      first = colored (singleton '[') bracketsColor
      last_ = colored (singleton ']') bracketsColor
      middle =
        snd
        $ foldl'
        (\(i,s) (ShotNumber n op) ->
            let num = intToDigit n
                opStr = case op of
                  Add -> ""
                  Substract -> "-"
                t = opStr <> case i of
                      0 -> singleton num
                      _ -> pack [num, ' ']
            in (i-1, s <> colored' t (numberColor n))) (lastIndex, first) nums

  in Successive [middle <> last_]

mkLeftInfo :: InfoType
           -> [BattleShip]
           -> Map ShipId Player
           -> [ShotNumber]
           -> LevelEssence
           -> [Successive ColorString]
mkLeftInfo t ships names shotNums (LevelEssence level (LevelTarget target _) _)=
  [ mkObjectiveCS t target
  , mkShotNumbersCS t shotNums
  ]
  ++
  map (mkShipCS t names) ships
  ++
  [ mkLevelCS t level
  ]

mkUpDownInfo :: (Successive ColorString, Successive ColorString)
mkUpDownInfo =
  (Successive [],Successive [])

mkInfos :: InfoType
        -> [BattleShip]
        -> Map ShipId Player
        -> [ShotNumber]
        -> LevelEssence
        -> ((Successive ColorString, Successive ColorString), [Successive ColorString])
mkInfos t ships names shotNums level =
  (mkUpDownInfo, mkLeftInfo t ships names shotNums level)
