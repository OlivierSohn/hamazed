{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Graphics.ParticleSystem.Chars
    ( niceChar
    -- * Reexports
    , Word8
    ) where

import           Imj.Prelude

-- | Returns one of the characters that /look good/ for explosive particle systems.
niceChar :: Word8 -- TODO should we retun 'Glyph's ?
         -- ^ We take the modulo of that value
         -> Char
niceChar i'
  | i == 0 = '$'
  | i == 1 = '?'
  | i == 2 = 'd'
  | i == 3 = 'R'
  | i == 4 = 'O'
  | i == 5 = 'o'
  | i == 6 = '^'
  | i == 7 = 'à'
  | i == 8 = '{'
  | i == 9 = '['
  | i == 10 = '|'
  | i == 11 = '!'
  | i == 12 = ':'
  | i == 13 = '¨'
  | i == 14 = '`'
  | i == 15 = '&'
  | i == 16 = '#'
  | i == 17 = '@'
  | i == 18 = '='
  | i == 19 = '\\'
  | i == 20 = '/'
  | i == 21 = '*'
  | otherwise = error $ "logic error in niceChar " ++ show i'
 where
   end = 22
   i = abs $ i' `mod` end

  -- characters by type:
  -- funny      : $
  -- surprising : ?
  -- beautiful  : d
  -- visible    : R
  -- bubbly     : O o
  -- birds      : ^ { [
  -- interesting: à
  -- subtle     : | !
  -- nice       : : & =
  -- very light : ¨ `
  -- heavy      : #
  -- round      : @
  -- sharp      : \ /
  -- rich       : *
