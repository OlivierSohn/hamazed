{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Music.Compositions.Vivaldi
      ( vivaldiFourSeasonsSpring
      , vivaldiFourSeasonsSummerPresto
      ) where

import           Imj.Prelude
import           Data.List(replicate, concat)

import           Imj.Music.CTypes
import           Imj.Music.Alter
import           Imj.Music.Compose

-- | Based on the beginning of Antonio Vivaldi's Four Seasons, Spring.
vivaldiFourSeasonsSpring :: (Float,[[VoiceInstruction]])
vivaldiFourSeasonsSpring = (bpm,part)
 where
  bpm = 350

  part = map (concat . replicate 4) $ map (map (transposeSymbol 4)) $ [voices|
    do -
    vsol -

    mi . mi . mi  .  ré  do  sol - -   - -  . sol fa
    do . do . vsi do vsi vla vsi . .   . ré . mi  ré

    mi . mi . mi . ré do sol ré sol - - mi sol fa
    do . do . vsi do vsi vla vsi vsol vsi do ré . mi ré

    mi . fa sol fa .   mi .   ré   .   vsi  .   vsol .
    do . ré mi  ré vsi do vla vsol vfa# vsol vfa# vsol .

    do -
    vsol -

    mi . mi . mi  .  ré  do  sol -   -  -  -  . sol fa
    do . do . vsi do vsi vla vsi - do do ré . mi  ré
    .  . .  . .   .  .   .   .   sol . sol .  ^do .

    mi sol mi . mi  .  ré  do  sol la   sol -  -  la sol fa
    do .   do . vsi do vsi vla vsi vsol vsi do ré .  mi  ré

    mi . fa sol fa .   mi .   ré   .   vsi  .   vsol .
    do . ré mi  ré vsi do vla vsol vfa# vsol vfa# vsol .
  |]

-- | Adapted from <https://www.youtube.com/watch?v=2gPPES89yXQ this partition>
vivaldiFourSeasonsSummerPresto :: (Float,[[VoiceInstruction]])
vivaldiFourSeasonsSummerPresto = (bpm,part)
 where
  bpm = 2500

  part = map (concat . replicate 4) $ map (map (transposeSymbol 7)) $ [voices|
    do - - . vdo - - . vdo - - . vdo - - . vdo - - . vdo - - . vdo - - . vdo - - . vdo - - . vdo - - . vdo - - . vdo - - .

    vsib - - . vdo - - . vdo - - . vdo - - . vdo - - . vdo - - . vdo - - . vdo - - . vdo - - . vdo - - . vdo - - . vdo - - .

    vlab - - . vdo - - . vdo - - . vdo - - . vdo - - . vdo - - . vdo - - . vdo - - . vdo - - . vdo - - . vdo - - . vdo - - .

    vsol - - . vdo - - . vdo - - . vdo - - . vdo - - . vdo - - . vdo - - . vdo - - . vdo - - . . . . . . . . . . . . .

    . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

    sol - - . vsol  - - . vsol  - - . vsol  - - . vsol  - - . vsol  - - . vsol  - - . vsol  - - . vsol  - - . vsol  - - . vsol  - - . vsol  - - .

    fa - - . vsol  - - . vsol  - - . vsol  - - . vsol  - - . vsol  - - . vsol  - - . vsol  - - . vsol  - - . vsol  - - . vsol  - - . vsol  - - .

    mib - - . vsol  - - . vsol  - - . vsol  - - . vsol  - - . vsol  - - . vsol  - - . vsol  - - . vsol  - - . vsol  - - . vsol  - - . vsol  - - .

    ré - - . vsol  - - . vsol  - - . vsol  - - . vsol  - - . vsol  - - . vsol  - - . vsol  - - . vsol  - - . . . . . . . . . . . . .

    . . . . sol - - . fa - - . mib - - . ré - - . do - - . vsi - - . vla - - . vsol - - . vfa - - . vmib - - . vré - - .

    . . . . sol - - . fa - - . mib - - . ré - - . do - - . vsi - - . vla - - . vsol - - . vfa - - . vmib - - . vré - - .
    vdo - - . . . . . vdo - - . . . . . vdo - - . . . . . vdo - - . . . . . vdo - - . . . . . vdo - - . . . . .
    . . . . vdo - - . . . . . vdo - - . . . . . vdo - - . . . . . vdo - - . . . . . vdo - - . . . . . vdo - - .

    . . . . ^do - - . sib - - . lab - - . sol - - . fa - - . mib - - . ré - - . do - - . vsib - - . vlab - - . vsol - - .
    vdo - - . . . . . vdo - - . . . . . vdo - - . . . . . vdo - - . . . . . vdo - - . . . . . vdo - - . . . . .
    . . . . vdo - - . . . . . vdo - - . . . . . vdo - - . . . . . vdo - - . . . . . vdo - - . . . . . vdo - - .

    . . . . ^do - - . sib - - . lab - - . sol - - . fa - - . mib - - . ré - - . do - - . vsib - - . vlab - - . vsol - - .
    vfa - - . . . . . vfa - - . . . . . vfa - - . . . . . vfa - - . . . . . vfa - - . . . . . vfa - - . . . . .
    . . . . vfa - - . . . . . vfa - - . . . . . vfa - - . . . . . vfa - - . . . . . vfa - - . . . . . vfa - - .

    . . . . ^mib - - . ^ré - - . ^do - - . sib - - . lab - - . sol - - . fa - - . mib - - . ré - - . do - - . vsib - - .
    vfa - - . . . . . vfa - - . . . . . vfa - - . . . . . vfa - - . . . . . vfa - - . . . . . vfa - - . . . . .
    . . . . vfa - - . . . . . vfa - - . . . . . vfa - - . . . . . vfa - - . . . . . vfa - - . . . . . vfa - - .

    . . . . ^mib - - . ^ré - - . ^do - - . sib - - . lab - - . sol - - . fa - - . mib - - . ré - - . do - - . vsib - - .
    vlab - - . . . . . vlab - - . . . . . vlab - - . . . . . vlab - - . . . . . vlab - - . . . . . vlab - - . . . . .
    . . . . vlab - - . . . . . vlab - - . . . . . vlab - - . . . . . vlab - - . . . . . vlab - - . . . . . vlab - - .

    . . . . ^sol - - . ^fa - - . ^mib - - . ^ré - - . ^do - - . si - - . la - - . sol - - . fa - - . mib - - . ré - - .
    vlab - - . . . . . vlab - - . . . . . vlab - - . . . . . vlab - - . . . . . vlab - - . . . . . vlab - - . . . . .
    . . . . vlab - - . . . . . vlab - - . . . . . vlab - - . . . . . vlab - - . . . . . vlab - - . . . . . vlab - - .

    . . . . ^sol - - . ^fa - - . ^mib - - . ^ré - - . ^do - - . si - - . la - - . sol - - . fa - - . mib - - . ré - - .
    do - - . . . . . do - - . . . . . do - - . . . . . do - - . . . . . do - - . . . . . do - - . . . . .
    . . . . do - - . . . . . do - - . . . . . do - - . . . . . do - - . . . . . do - - . . . . . do - - .

    . . . . ^sol - - . ^fa - - . ^mib - - . ^ré - - . ^do - - . si - - . la - - . sol - - . fa - - . mib - - . ré - - .
    do - - . . . . . do - - . . . . . do - - . . . . . do - - . . . . . do - - . . . . . do - - . . . . .
    . . . . do - - . . . . . do - - . . . . . do - - . . . . . do - - . . . . . do - - . . . . . do - - .

    . . . . ^sol - - . ^fa - - . ^mib - - . ^ré - - . ^do - - . si - - . la - - . sol - - . fa - - . mib - - . ré - - .
    do - - . . . . . do - - . . . . . do - - . . . . . do - - . . . . . do - - . . . . . do - - . . . . .
    . . . . do - - . . . . . do - - . . . . . do - - . . . . . do - - . . . . . do - - . . . . . do - - .

    do - - . . . . . do - - . . . . . sol - - . . . . . do - - . . . . . sol - - . . . . . do - - . . . . .
    . . . . do - - . . . . . do - - . . . . . do - - . . . . . do - - . . . . . do - - . . . . . do - - .

    vsi - - . ré - - . mib - - . fa - - . sol - - . la - - . si - - . ^do - - . ^ré - - . sol - - . ré - - . vsi - - .
    . . . . vsol - - . . . . . vsol - - . . . . . vsol - - . . . . . vsol - - . . . . . vsol - - . . . . . vsol - - .
    vsol - - . . . . . vsol - - . . . . . vsol - - . . . . . vsol - - . . . . . vsol - - . . . . . vsol - - . . . . .

    vsol - - . ré - - . mib - - . fa - - . sol - - . la - - . si - - . ^do - - . ^ré - - . sol - - . ré - - . vsi - - .
    . . . . vfa - - . . . . . vfa - - . . . . . vfa - - . . . . . vfa - - . . . . . vfa - - . . . . . vfa - - .
    vfa - - . . . . . vfa - - . . . . . vfa - - . . . . . vfa - - . . . . . vfa - - . . . . . vfa - - . . . . .

    do - - . mib - - . fa - - . sol - - . la - - . si - - . ^do - - . ^ré - - . ^mib - - . la - - . mib - - . do - - .
    . . . . vmib - - . . . . . vmib - - . . . . . vmib - - . . . . . vmib - - . . . . . vmib - - . . . . . vmib - - .
    vmib - - . . . . . vmib - - . . . . . vmib - - . . . . . vmib - - . . . . . vmib - - . . . . . vmib - - . . . . .

    vsib - - . sol - - . la - - . si - - . ^do - - . ^ré - - . ^mib - - . ^fa - - . ^sol - - . sol - - . ré - - . vsi - - .
    . . . . vré - - . . . . . vré - - . . . . . vré - - . . . . . vré - - . . . . . vré - - . . . . . vré - - .
    vré - - . . . . . vré - - . . . . . vré - - . . . . . vré - - . . . . . vré - - . . . . . vré - - . . . . .

    vsol - - . sol - - . la - - . si - - . ^do - - . ^ré - - . ^mib - - . ^fa - - . ^sol - - . sol - - . ré - - . vsi - - .
    . . . . vsol - - . . . . . vsol - - . . . . . vsol - - . . . . . vsol - - . . . . . vsol - - . . . . . vsol - - .
    vsol - - . . . . . vsol - - . . . . . vsol - - . . . . . vsol - - . . . . . vsol - - . . . . . vsol - - . . . . .

    vsol - - . ré - - . mib - - . fa - - . sol - - . la - - . si - - . ^do - - . ^ré - - . sol - - . ré - - . vsi - - .
    . . . . vfa - - . . . . . vfa - - . . . . . vfa - - . . . . . vfa - - . . . . . vfa - - . . . . . vfa - - .
    vfa - - . . . . . vfa - - . . . . . vfa - - . . . . . vfa - - . . . . . vfa - - . . . . . vfa - - . . . . .

    do - - . mib - - . fa - - . sol - - . la - - . si - - . ^do - - . ^ré - - . ^mib - - . la - - . mib - - . do - - .
    . . . . vmib - - . . . . . vmib - - . . . . . vmib - - . . . . . vmib - - . . . . . vmib - - . . . . . vmib - - .
    vmib - - . . . . . vmib - - . . . . . vmib - - . . . . . vmib - - . . . . . vmib - - . . . . . vmib - - . . . . .

    vsib - - . sol - - . la - - . si - - . ^do - - . ^ré - - . ^mib - - . ^fa - - . ^sol - - . sol - - . ré - - . vsi - - .
    . . . . vré - - . . . . . vré - - . . . . . vré - - . . . . . vré - - . . . . . vré - - . . . . . vré - - .
    vré - - . . . . . vré - - . . . . . vré - - . . . . . vré - - . . . . . vré - - . . . . . vré - - . . . . .

    ^mib - - . ^do - - . sol - - . mib - - . ^mib - - . ^do - - . sol - - . mib - - . ^mib - - . ^do - - . sol - - . mib - - .
    vdo - - . . . . . vdo - - . . . . . vdo - - . . . . . vdo - - . . . . . vdo - - . . . . . vdo - - . . . . .

    sol - - . mib - - . do - - . vsol - - . sol - - . mib - - . do - - . vsol - - . sol - - . mib - - . do - - . vsol - - .
    vdo - - . . . . . vdo - - . . . . . vdo - - . . . . . vdo - - . . . . . vdo - - . . . . . vdo - - . . . . .

    do - - . vsol - - . vmib - - . vdo - - . do - - . vsol - - . vmib - - . vdo - - . do - - . vsol - - . vmib - - . vdo - - .
    vdo - - . . . . . vdo - - . . . . . vdo - - . . . . . vdo - - . . . . . vdo - - . . . . . vdo - - . . . . .

    do - - . mi - - . do - - . mi - - . do - - . mi - - . do - - . mi - - . do - - . mi - - . do - - . mi - - .
    vdo - - . . . . . vdo - - . . . . . vdo - - . . . . . vdo - - . . . . . vdo - - . . . . . vdo - - . . . . .

    do - - . fa - - . do - - . fa - - . do - - . fa - - . do - - . fa - - . do - - . fa - - . do - - . fa - - .
    vfa - - . . . . . vfa - - . . . . . vfa - - . . . . . vfa - - . . . . . vfa - - . . . . . vfa - - . . . . .

    do - - . fa# - - . do - - . fa# - - . do - - . fa# - - . do - - . fa# - - . do - - . fa# - - . do - - . fa# - - .
    vré - - . . . . . vré - - . . . . . vré - - . . . . . vré - - . . . . . vré - - . . . . . vré - - . . . . .

    ré - - . sol - - . ré - - . sol - - . ré - - . sol - - . ré - - . sol - - . ré - - . sol - - . ré - - . sol - - .
    vsol - - . . . . . vsol - - . . . . . vsol - - . . . . . vsol - - . . . . . vsol - - . . . . . vsol - - . . . . .

    ré - - . sol - - . ré - - . sol - - . ré - - . sol - - . ré - - . sol - - . ré - - . sol - - . ré - - . sol - - .
    vré - - . . . . . vré - - . . . . . vré - - . . . . . vré - - . . . . . vré - - . . . . . vré - - . . . . .

    ré - - . fa# - - . ré - - . fa# - - . ré - - . fa# - - . ré - - . fa# - - . ré - - . fa# - - . ré - - . fa# - - .
    vré - - . . . . . vré - - . . . . . vré - - . . . . . vré - - . . . . . vré - - . . . . . vré - - . . . . .

    sol - - . vsol - . . vla - . . vsib - . . do - . . ré - . . mib - . . fa# - . . sol - . . ré - . . mib - . . fa# - . .
    vsol - - .

    sol - . . la - . . sib - . . ^do - . . ^ré - . . sol - . . la - . . sib - . . ^do - . . ^ré - . . ^mi - . . ^fa# - . .

    ^sol - - -  - - - .  . . . .  . . . .  . . . . ^ré - - . ^ré - - . ^fa# - - . ^fa# - - . ^la - - . ^la - - . ^^ré - - .
    vsol - - .

   ^^ré - - . ^ré - - . vsol - - . ^ré - - . ^^do - - . ^ré - - . vsol - - . ^ré - - . ^sib - - . ^ré - - . vsol - - . ^ré - - .
    vsol - - . . . . . vsol - - . . . . . vsol - - . . . . . vsol - - . . . . . vsol - - . . . . . vsol - - . . . . .

   ^la - - . ^ré - - . vsol - - . ^ré - - . ^sib - - . ^ré - - . vsol - - . ^ré - - . ^^do - - . ^ré - - . vsol - - . ^ré - - .
    vsol - - . . . . . vsol - - . . . . . vsol - - . . . . . vsol - - . . . . . vsol - - . . . . . vsol - - . . . . .

   ^^ré - - . ^ré - - . vsol - - . ^ré - - . ^^do - - . ^ré - - . vsol - - . ^ré - - . ^sib - - . ^ré - - . vsol - - . ^ré - - .
    vsol - - . . . . . vsol - - . . . . . vsol - - . . . . . vsol - - . . . . . vsol - - . . . . . vsol - - . . . . .

   ^la - - -  - - - .  . . . .  . . . .  . . . . sol - - . sol - - . sib - - . sib - - . ^ré - - . ^ré - - . ^sol - - .

   ^sol - - . sol - - . vsol - - . sol - - . ^fa - - . sol - - . vsol - - . sol - - . ^mib - - . sol - - . vsol - - . sol - - .
    vsol - - . . . . . vsol - - . . . . . vsol - - . . . . . vsol - - . . . . . vsol - - . . . . . vsol - - . . . . .

   ^ré - - . sol - - . vsol - - . sol - - . ^mib - - . sol - - . vsol - - . sol - - . ^fa# - - . sol - - . vsol - - . sol - - .
    vsol - - . . . . . vsol - - . . . . . vsol - - . . . . . vsol - - . . . . . vsol - - . . . . . vsol - - . . . . .

   ^sol - - . sol - - . vsol - - . sol - - . ^fa - - . sol - - . vsol - - . sol - - . ^mib - - . sol - - . vsol - - . sol - - .
    vsol - - . . . . . vsol - - . . . . . vsol - - . . . . . vsol - - . . . . . vsol - - . . . . . vsol - - . . . . .

   ^ré - - -  - - - .    . . . .    . . . .   . . . .   ^^ré - - . ^^ré - - . ^^do - - . ^^do - - . ^sib - - . ^sib - - . ^la - - .

   ^la - - . ^sol - - . ^sol - - . ^fa - - . ^fa - - . ^mib - - . ^mib - - . ^ré - - . ^ré - - . ^do - - . ^do - - . sib - - .

   sib - - .  la - - .   la - - . sol - - . sol - - . fa - - . fa - - . mib - - . mib - - . ré - - . ré - - . do# - - .

   ré - - -  - - - . do# - - -  - - - . ré - - -  - - - . do# - - -  - - - . ré - - -  - - - . vsib - - -  - - - .

   : to be continued...

   ré - . .  . . . . do#  - -  - - - . ré - - -  - - - . do# - - -  - - - . ré - - -  - - - . vsib - - -  - - - .

   ré - . .  . . . . do# - . .  . . . . ré - - -  - - - . do# - - -  - - - . ré - . .  . . . .  . . . .  . . . .
  |]
