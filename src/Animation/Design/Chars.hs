{-# LANGUAGE NoImplicitPrelude #-}

module Animation.Design.Chars
    ( niceCharsForExplosions
    ) where

import           Imajuscule.Prelude

import           Data.Text(pack)

niceCharsForExplosions :: Text
niceCharsForExplosions = pack "$?dROo^à{[|!:¨`&#@=-\\/*"

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
