{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Music.Compositions.Vivaldi
      ( vivaldiFourSeasonsSpring
      , vivaldiFourSeasonsSummerPresto
      ) where

import           Imj.Prelude

import           Imj.Music.Instruction
import           Imj.Music.Instrument(NotePan(..), allCentered)
import           Imj.Music.Alter
import           Imj.Music.Compose

-- | The beginning of <https://www.youtube.com/watch?v=e3nSvIiBNFo the 1st movement>
-- from Antonio Vivaldi's Four Seasons / Spring.
vivaldiFourSeasonsSpring :: (Double,[(NotePan, [Instruction])])
vivaldiFourSeasonsSpring = (bpm,part)
 where
  bpm = 350

  part = allCentered $ map (map (transposeSymbol 4)) $ [voices|
    do -
    vsol -

    mi . mi . mi  .  re  do  sol - -   - -  . sol fa
    do . do . vsi do vsi vla vsi . .   . re . mi  re

    mi . mi . mi  .  re  do  sol re   sol -  -  mi sol fa
    do . do . vsi do vsi vla vsi vsol vsi do re .  mi  re

    mi . fa sol fa .   mi .   re   .    vsi  .    vsol .
    do . re mi  re vsi do vla vsol vfa# vsol vfa# vsol .

    do   -
    vsol -

    mi . mi . mi  .  re  do  sol -   -  -   -  .   sol fa
    do . do . vsi do vsi vla vsi -   do do  re .   mi  re
    .  . .  . .   .  .   .   .   sol .  sol .  ^do .

    mi sol mi . mi  .  re  do  sol la   sol -  -  la sol fa
    do .   do . vsi do vsi vla vsi vsol vsi do re .  mi  re

    mi . fa sol fa .   mi .   re   .    vsi  .    vsol .
    do . re mi  re vsi do vla vsol vfa# vsol vfa# vsol .
  |]

-- | The beginning of <https://www.youtube.com/watch?v=124NoPUBDvA Presto>,
-- from Antonio Vivaldi's Four Seasons / Summer.
vivaldiFourSeasonsSummerPresto :: (Double,[(NotePan, [Instruction])])
vivaldiFourSeasonsSummerPresto = (bpm,part)
 where
  bpm = 2500

  part = allCentered $ map (map (transposeSymbol 7)) $ [voices|
    do - - . vdo - - . vdo - - . vdo - - . vdo - - . vdo - - . vdo - - . vdo - - . vdo - - . vdo - - . vdo - - . vdo - - .

    vsib - - . vdo - - . vdo - - . vdo - - . vdo - - . vdo - - . vdo - - . vdo - - . vdo - - . vdo - - . vdo - - . vdo - - .

    vlab - - . vdo - - . vdo - - . vdo - - . vdo - - . vdo - - . vdo - - . vdo - - . vdo - - . vdo - - . vdo - - . vdo - - .

    vsol - - . vdo - - . vdo - - . vdo - - . vdo - - . vdo - - . vdo - - . vdo - - . vdo - - . . . . . . . . . . . . .

    . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

    sol - - . vsol  - - . vsol  - - . vsol  - - . vsol  - - . vsol  - - . vsol  - - . vsol  - - . vsol  - - . vsol  - - . vsol  - - . vsol  - - .

    fa - - . vsol  - - . vsol  - - . vsol  - - . vsol  - - . vsol  - - . vsol  - - . vsol  - - . vsol  - - . vsol  - - . vsol  - - . vsol  - - .

    mib - - . vsol  - - . vsol  - - . vsol  - - . vsol  - - . vsol  - - . vsol  - - . vsol  - - . vsol  - - . vsol  - - . vsol  - - . vsol  - - .

    re - - . vsol  - - . vsol  - - . vsol  - - . vsol  - - . vsol  - - . vsol  - - . vsol  - - . vsol  - - . . . . . . . . . . . . .

    . . . . sol - - . fa - - . mib - - . re - - . do - - . vsi - - . vla - - . vsol - - . vfa - - . vmib - - . vre - - .

    . . . . sol - - . fa - - . mib - - . re - - . do - - . vsi - - . vla - - . vsol - - . vfa - - . vmib - - . vre - - .
    vdo - - . . . . . vdo - - . . . . . vdo - - . . . . . vdo - - . . . . . vdo - - . . . . . vdo - - . . . . .
    . . . . vdo - - . . . . . vdo - - . . . . . vdo - - . . . . . vdo - - . . . . . vdo - - . . . . . vdo - - .

    . . . . ^do - - . sib - - . lab - - . sol - - . fa - - . mib - - . re - - . do - - . vsib - - . vlab - - . vsol - - .
    vdo - - . . . . . vdo - - . . . . . vdo - - . . . . . vdo - - . . . . . vdo - - . . . . . vdo - - . . . . .
    . . . . vdo - - . . . . . vdo - - . . . . . vdo - - . . . . . vdo - - . . . . . vdo - - . . . . . vdo - - .

    . . . . ^do - - . sib - - . lab - - . sol - - . fa - - . mib - - . re - - . do - - . vsib - - . vlab - - . vsol - - .
    vfa - - . . . . . vfa - - . . . . . vfa - - . . . . . vfa - - . . . . . vfa - - . . . . . vfa - - . . . . .
    . . . . vfa - - . . . . . vfa - - . . . . . vfa - - . . . . . vfa - - . . . . . vfa - - . . . . . vfa - - .

    . . . . ^mib - - . ^re - - . ^do - - . sib - - . lab - - . sol - - . fa - - . mib - - . re - - . do - - . vsib - - .
    vfa - - . . . . . vfa - - . . . . . vfa - - . . . . . vfa - - . . . . . vfa - - . . . . . vfa - - . . . . .
    . . . . vfa - - . . . . . vfa - - . . . . . vfa - - . . . . . vfa - - . . . . . vfa - - . . . . . vfa - - .

    . . . . ^mib - - . ^re - - . ^do - - . sib - - . lab - - . sol - - . fa - - . mib - - . re - - . do - - . vsib - - .
    vlab - - . . . . . vlab - - . . . . . vlab - - . . . . . vlab - - . . . . . vlab - - . . . . . vlab - - . . . . .
    . . . . vlab - - . . . . . vlab - - . . . . . vlab - - . . . . . vlab - - . . . . . vlab - - . . . . . vlab - - .

    . . . . ^sol - - . ^fa - - . ^mib - - . ^re - - . ^do - - . si - - . la - - . sol - - . fa - - . mib - - . re - - .
    vlab - - . . . . . vlab - - . . . . . vlab - - . . . . . vlab - - . . . . . vlab - - . . . . . vlab - - . . . . .
    . . . . vlab - - . . . . . vlab - - . . . . . vlab - - . . . . . vlab - - . . . . . vlab - - . . . . . vlab - - .

    . . . . ^sol - - . ^fa - - . ^mib - - . ^re - - . ^do - - . si - - . la - - . sol - - . fa - - . mib - - . re - - .
    do - - . . . . . do - - . . . . . do - - . . . . . do - - . . . . . do - - . . . . . do - - . . . . .
    . . . . do - - . . . . . do - - . . . . . do - - . . . . . do - - . . . . . do - - . . . . . do - - .

    . . . . ^sol - - . ^fa - - . ^mib - - . ^re - - . ^do - - . si - - . la - - . sol - - . fa - - . mib - - . re - - .
    do - - . . . . . do - - . . . . . do - - . . . . . do - - . . . . . do - - . . . . . do - - . . . . .
    . . . . do - - . . . . . do - - . . . . . do - - . . . . . do - - . . . . . do - - . . . . . do - - .

    . . . . ^sol - - . ^fa - - . ^mib - - . ^re - - . ^do - - . si - - . la - - . sol - - . fa - - . mib - - . re - - .
    do - - . . . . . do - - . . . . . do - - . . . . . do - - . . . . . do - - . . . . . do - - . . . . .
    . . . . do - - . . . . . do - - . . . . . do - - . . . . . do - - . . . . . do - - . . . . . do - - .

    do - - . . . . . do - - . . . . . sol - - . . . . . do - - . . . . . sol - - . . . . . do - - . . . . .
    . . . . do - - . . . . . do - - . . . . . do - - . . . . . do - - . . . . . do - - . . . . . do - - .

    vsi - - . re - - . mib - - . fa - - . sol - - . la - - . si - - . ^do - - . ^re - - . sol - - . re - - . vsi - - .
    . . . . vsol - - . . . . . vsol - - . . . . . vsol - - . . . . . vsol - - . . . . . vsol - - . . . . . vsol - - .
    vsol - - . . . . . vsol - - . . . . . vsol - - . . . . . vsol - - . . . . . vsol - - . . . . . vsol - - . . . . .

    vsol - - . re - - . mib - - . fa - - . sol - - . la - - . si - - . ^do - - . ^re - - . sol - - . re - - . vsi - - .
    . . . . vfa - - . . . . . vfa - - . . . . . vfa - - . . . . . vfa - - . . . . . vfa - - . . . . . vfa - - .
    vfa - - . . . . . vfa - - . . . . . vfa - - . . . . . vfa - - . . . . . vfa - - . . . . . vfa - - . . . . .

    do - - . mib - - . fa - - . sol - - . la - - . si - - . ^do - - . ^re - - . ^mib - - . la - - . mib - - . do - - .
    . . . . vmib - - . . . . . vmib - - . . . . . vmib - - . . . . . vmib - - . . . . . vmib - - . . . . . vmib - - .
    vmib - - . . . . . vmib - - . . . . . vmib - - . . . . . vmib - - . . . . . vmib - - . . . . . vmib - - . . . . .

    vsib - - . sol - - . la - - . si - - . ^do - - . ^re - - . ^mib - - . ^fa - - . ^sol - - . sol - - . re - - . vsi - - .
    . . . . vre - - . . . . . vre - - . . . . . vre - - . . . . . vre - - . . . . . vre - - . . . . . vre - - .
    vre - - . . . . . vre - - . . . . . vre - - . . . . . vre - - . . . . . vre - - . . . . . vre - - . . . . .

    vsol - - . sol - - . la - - . si - - . ^do - - . ^re - - . ^mib - - . ^fa - - . ^sol - - . sol - - . re - - . vsi - - .
    . . . . vsol - - . . . . . vsol - - . . . . . vsol - - . . . . . vsol - - . . . . . vsol - - . . . . . vsol - - .
    vsol - - . . . . . vsol - - . . . . . vsol - - . . . . . vsol - - . . . . . vsol - - . . . . . vsol - - . . . . .

    vsol - - . re - - . mib - - . fa - - . sol - - . la - - . si - - . ^do - - . ^re - - . sol - - . re - - . vsi - - .
    . . . . vfa - - . . . . . vfa - - . . . . . vfa - - . . . . . vfa - - . . . . . vfa - - . . . . . vfa - - .
    vfa - - . . . . . vfa - - . . . . . vfa - - . . . . . vfa - - . . . . . vfa - - . . . . . vfa - - . . . . .

    do - - . mib - - . fa - - . sol - - . la - - . si - - . ^do - - . ^re - - . ^mib - - . la - - . mib - - . do - - .
    . . . . vmib - - . . . . . vmib - - . . . . . vmib - - . . . . . vmib - - . . . . . vmib - - . . . . . vmib - - .
    vmib - - . . . . . vmib - - . . . . . vmib - - . . . . . vmib - - . . . . . vmib - - . . . . . vmib - - . . . . .

    vsib - - . sol - - . la - - . si - - . ^do - - . ^re - - . ^mib - - . ^fa - - . ^sol - - . sol - - . re - - . vsi - - .
    . . . . vre - - . . . . . vre - - . . . . . vre - - . . . . . vre - - . . . . . vre - - . . . . . vre - - .
    vre - - . . . . . vre - - . . . . . vre - - . . . . . vre - - . . . . . vre - - . . . . . vre - - . . . . .

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
    vre - - . . . . . vre - - . . . . . vre - - . . . . . vre - - . . . . . vre - - . . . . . vre - - . . . . .

    re - - . sol - - . re - - . sol - - . re - - . sol - - . re - - . sol - - . re - - . sol - - . re - - . sol - - .
    vsol - - . . . . . vsol - - . . . . . vsol - - . . . . . vsol - - . . . . . vsol - - . . . . . vsol - - . . . . .

    re - - . sol - - . re - - . sol - - . re - - . sol - - . re - - . sol - - . re - - . sol - - . re - - . sol - - .
    vre - - . . . . . vre - - . . . . . vre - - . . . . . vre - - . . . . . vre - - . . . . . vre - - . . . . .

    re - - . fa# - - . re - - . fa# - - . re - - . fa# - - . re - - . fa# - - . re - - . fa# - - . re - - . fa# - - .
    vre - - . . . . . vre - - . . . . . vre - - . . . . . vre - - . . . . . vre - - . . . . . vre - - . . . . .

    sol - - . vsol - . . vla - . . vsib - . . do - . . re - . . mib - . . fa# - . . sol - . . re - . . mib - . . fa# - . .
    vsol - - .

    sol - . . la - . . sib - . . ^do - . . ^re - . . sol - . . la - . . sib - . . ^do - . . ^re - . . ^mi - . . ^fa# - . .

    ^sol - - -  - - - .  . . . .  . . . .  . . . . ^re - - . ^re - - . ^fa# - - . ^fa# - - . ^la - - . ^la - - . ^^re - - .
    vsol - - .

   ^^re - - . ^re - - . vsol - - . ^re - - . ^^do - - . ^re - - . vsol - - . ^re - - . ^sib - - . ^re - - . vsol - - . ^re - - .
    vsol - - . . . . . vsol - - . . . . . vsol - - . . . . . vsol - - . . . . . vsol - - . . . . . vsol - - . . . . .

   ^la - - . ^re - - . vsol - - . ^re - - . ^sib - - . ^re - - . vsol - - . ^re - - . ^^do - - . ^re - - . vsol - - . ^re - - .
    vsol - - . . . . . vsol - - . . . . . vsol - - . . . . . vsol - - . . . . . vsol - - . . . . . vsol - - . . . . .

   ^^re - - . ^re - - . vsol - - . ^re - - . ^^do - - . ^re - - . vsol - - . ^re - - . ^sib - - . ^re - - . vsol - - . ^re - - .
    vsol - - . . . . . vsol - - . . . . . vsol - - . . . . . vsol - - . . . . . vsol - - . . . . . vsol - - . . . . .

   ^la - - -  - - - .  . . . .  . . . .  . . . . sol - - . sol - - . sib - - . sib - - . ^re - - . ^re - - . ^sol - - .

   ^sol - - . sol - - . vsol - - . sol - - . ^fa - - . sol - - . vsol - - . sol - - . ^mib - - . sol - - . vsol - - . sol - - .
    vsol - - . . . . . vsol - - . . . . . vsol - - . . . . . vsol - - . . . . . vsol - - . . . . . vsol - - . . . . .

   ^re - - . sol - - . vsol - - . sol - - . ^mib - - . sol - - . vsol - - . sol - - . ^fa# - - . sol - - . vsol - - . sol - - .
    vsol - - . . . . . vsol - - . . . . . vsol - - . . . . . vsol - - . . . . . vsol - - . . . . . vsol - - . . . . .

   ^sol - - . sol - - . vsol - - . sol - - . ^fa - - . sol - - . vsol - - . sol - - . ^mib - - . sol - - . vsol - - . sol - - .
    vsol - - . . . . . vsol - - . . . . . vsol - - . . . . . vsol - - . . . . . vsol - - . . . . . vsol - - . . . . .

   ^re - - -  - - - .    . . . .    . . . .   . . . .   ^^re - - . ^^re - - . ^^do - - . ^^do - - . ^sib - - . ^sib - - . ^la - - .

   ^la - - . ^sol - - . ^sol - - . ^fa - - . ^fa - - . ^mib - - . ^mib - - . ^re - - . ^re - - . ^do - - . ^do - - . sib - - .

   sib - - .  la - - .   la - - . sol - - . sol - - . fa - - . fa - - . mib - - . mib - - . re - - . re - - . do# - - .

   re - - -  - - - . do# - - -  - - - . re - - -  - - - . do# - - -  - - - . re - - -  - - - . vsib - - -  - - - .

   : to be continued...

   re - . .  . . . . do#  - -  - - - . re - - -  - - - . do# - - -  - - - . re - - -  - - - . vsib - - -  - - - .

   re - . .  . . . . do# - . .  . . . . re - - -  - - - . do# - - -  - - - . re - . .  . . . .  . . . .  . . . .
  |]
