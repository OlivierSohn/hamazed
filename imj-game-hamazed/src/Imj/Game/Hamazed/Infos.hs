{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Imj.Game.Hamazed.Infos
      ( mkInfos
      , mkLeftInfo
      , InfoType(..)
      ) where

import           Imj.Prelude

import           Data.Char( intToDigit )
import           Data.Map.Strict((!?))
import qualified Data.Map.Strict as Map

import           Imj.Game.Hamazed.Network.Types
import           Imj.Game.Hamazed.Types

import           Imj.Game.Color
import           Imj.Game.Hamazed.Color
import           Imj.Graphics.Class.Words
import           Imj.Graphics.Font
import           Imj.Graphics.Text.ColoredGlyphList

data InfoType = Normal | ColorAnimated

text :: String -> ColoredGlyphList
text x = text' x configFgColor

text' :: String -> Color8 Foreground -> ColoredGlyphList
text' x = colored (map textGlyph x)

number :: String -> Color8 Foreground -> ColoredGlyphList
number x = colored (map gameGlyph x)

mkLevelCS :: InfoType -> LevelNumber -> Successive ColoredGlyphList
mkLevelCS t (LevelNumber level) =
  let neutralColor = neutralMessageColorFg
      (LevelNumber lastLev) = lastLevel
      txt x = text "Level " <> number (show level) x <> text " / " <> number (show lastLev) neutralColor
  in Successive $ map txt $ case t of
      Normal        -> [neutralColor]
      ColorAnimated -> [black, neutralColor]

mkShipCS :: InfoType
         -> Map ShipId Player -- TODO use the (AppState s) monad instead of passing this ?
         -> ShipId
         -> BattleShip
         -> Successive ColoredGlyphList
mkShipCS _ names sid (BattleShip _ ammo status _ _) =
  let name = getPlayerUIName'' $ names !? sid
      pad = initialLaserAmmo - ammo
      ammoColor' Destroyed = darkConfigFgColor
      ammoColor' _   = ammoColor

      s = name
       <> text ("   " ++ replicate pad ' ')
       <> insideBrackets (text' (replicate ammo '.') (ammoColor' status))
   in Successive [s]

mkObjectiveCS :: InfoType -> Int -> Successive ColoredGlyphList
mkObjectiveCS t target =
  let txt x = text "Objective : " <> x
      n c = number (show target) c
  in Successive $ case t of
    Normal -> [txt $ n white]
    ColorAnimated -> map txt ["", n black, n white]


mkShotNumbersCS :: InfoType -> [ShotNumber] -> Successive ColoredGlyphList
mkShotNumbersCS _ nums =
  let middle = unwords $
        map (\(ShotNumber n op) ->
              let t = case op of
                    Add -> mempty
                    Substract -> "-"
              in number (t ++ [intToDigit n]) $ numberColor n)
          nums
  in Successive [insideBrackets middle]

insideBrackets :: ColoredGlyphList -> ColoredGlyphList
insideBrackets a =
  text' "[" bracketsColor <>
  a <>
  text' "]" bracketsColor

mkLeftInfo :: InfoType
           -> Map ShipId BattleShip
           -> Map ShipId Player
           -> [ShotNumber]
           -> LevelEssence
           -> [Successive ColoredGlyphList]
mkLeftInfo t ships names shotNums (LevelEssence level (LevelTarget target _) _)=
  [ mkObjectiveCS t target
  , mkShotNumbersCS t shotNums
  ]
  ++
  case Map.assocs ships of
    [] -> [Successive [colorize (onBlack $ gray 10) $ fromString ""]] -- for initial state when we were not given an id yet
    l -> map (uncurry $ mkShipCS t names) l
  ++
  [ mkLevelCS t level
  ]

mkUpDownInfo :: (Successive ColoredGlyphList, Successive ColoredGlyphList)
mkUpDownInfo =
  (Successive [],Successive [])

mkInfos :: InfoType
        -> Map ShipId BattleShip
        -> Map ShipId Player
        -> [ShotNumber]
        -> LevelEssence
        -> ((Successive ColoredGlyphList, Successive ColoredGlyphList), [Successive ColoredGlyphList])
mkInfos t ships names shotNums level =
  (mkUpDownInfo, mkLeftInfo t ships names shotNums level)
