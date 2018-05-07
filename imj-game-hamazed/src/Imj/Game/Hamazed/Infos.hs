{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Imj.Game.Hamazed.Infos
      ( mkUpDownInfo
      , mkLeftUpInfo
      , mkLeftDownInfo
      ) where

import           Imj.Prelude

import           Data.Char(intToDigit)

import           Imj.Game.Hamazed.Network.Types
import           Imj.Game.Hamazed.World.Types

import           Imj.Game.Color
import           Imj.Game.Infos
import           Imj.Game.Hamazed.Color
import           Imj.Game.Hamazed.Level
import           Imj.Graphics.Class.Words
import           Imj.Graphics.Font
import           Imj.Graphics.Text.ColoredGlyphList

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

mkLeftUpInfo :: InfoType
             -> [ShotNumber]
             -> LevelEssence
             -> [Successive ColoredGlyphList]
mkLeftUpInfo t shotNums (LevelEssence _ (LevelTarget target _) _) =
  [ mkObjectiveCS t target, mkShotNumbersCS t shotNums ]


mkLeftDownInfo :: InfoType
               -> LevelEssence
               -> [Successive ColoredGlyphList]
mkLeftDownInfo t (LevelEssence level _ _) =
  [ mkLevelCS t level ]

mkUpDownInfo :: (Successive ColoredGlyphList, Successive ColoredGlyphList)
mkUpDownInfo =
  (Successive [""],Successive [""])
