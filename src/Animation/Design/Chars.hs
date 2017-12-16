{-# LANGUAGE NoImplicitPrelude #-}

-- | This module is about characters we can use for explosive animations

module Animation.Design.Chars
    ( niceChar
    ) where

import           Imajuscule.Prelude

import           Data.Array.Unboxed( Array, listArray, bounds, (!) )
import           Data.List(length)


-- | Returns one of the "nice chars" for animations
niceChar :: Int -> Char
niceChar i =
  let l = 1 + snd (bounds niceChars)
      index = abs $ i `mod` l
  in niceChars ! index

-- | These chars work well for explosive animations
niceChars :: Array Int Char
niceChars = listArray (0, length chars - 1) chars
  where chars = "$?dROo^à{[|!:¨`&#@=\\/*"

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
