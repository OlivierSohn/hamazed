{-# LANGUAGE DeriveGeneric #-}

module Imj.Music.PressedKeys
      ( onMusic
      , releaseAllKeys
      , addNote
      , removeNote
      ) where

import qualified Data.Map.Strict as Map

import           Imj.Music.Types
import           Imj.Music.Instrument

{-# INLINABLE addNote #-}
addNote :: Ord i
        => InstrumentNote i -> PressedKeys i -> (Int, PressedKeys i)
addNote n (PressedKeys s) = (1, PressedKeys $ Map.insertWith (+) n 1 s)

{-# INLINABLE removeNote #-}
removeNote :: Ord i
           => InstrumentNote i -> PressedKeys i -> (Int,PressedKeys i)
removeNote n (PressedKeys s) =
  (maybe 0 (const 1) mayNote, PressedKeys m)
 where
  (mayNote, m) =
    Map.updateLookupWithKey -- mayNote is Nothing only if the value was not in the map.
      (\_ prev ->
        case prev - 1 of
          0 -> Nothing -- mayNote will /not/ be Nothing in that case.
          i ->
            if i > 0
              then
                Just i
              else
                error "logic")
      n
      s

-- | Returns the [MusicalEvent] such that the piano plays no note.
releaseAllKeys :: PressedKeys i -> [MusicalEvent i]
releaseAllKeys (PressedKeys m) =
  concatMap (\(n,i) -> replicate i (StopNote Nothing n)) $ Map.assocs m

{-# INLINABLE onMusic #-}
onMusic :: Ord i
        => MusicalEvent i
        -> PressedKeys i
        -> (Int,PressedKeys i)
        -- ^ returns the number of changed notes (added or removed)
onMusic n = case n of
  StartNote _ spec _ -> addNote spec
  StopNote _ spec -> removeNote spec
