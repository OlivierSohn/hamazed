{-# LANGUAGE NoImplicitPrelude #-}

module Animation.Design.Chars
    ( niceChar
    ) where

import           Imajuscule.Prelude

import           Data.Array.Unboxed( Array, listArray, bounds, (!) )
import           Data.List(length)


niceChar :: Int -> Char
niceChar i =
  let l = 1 + snd (bounds niceChars)
      index = abs $ i `mod` l
  in niceChars ! index

-- | These chars work well for explosive animations
niceChars :: Array Int Char
niceChars = listArray (0, length chars - 1) chars
  where chars = "$?dROo^à{[|!:¨`&#@=\\/*"

  -- dollar   is funny
  -- ?   is surprising
  -- d   is beautiful
  -- R   is visible
  -- O   is bubbly
  -- o   too
  -- ^   is birds
  -- à   is interesting
  -- {   is birds too
  -- [   is craws
  -- |   is subtle
  -- !   is subtle
  -- :   is nice
  -- ¨   is very light
  -- `   is very light
  -- &   is nice
  -- #   is heavy
  -- @   is round
  -- =   is nice too
  -- -   is light
  -- \   is sharp
  -- /   is sharp
  -- *   is rich
