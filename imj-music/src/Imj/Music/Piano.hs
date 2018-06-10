{-# LANGUAGE DeriveGeneric #-}

module Imj.Music.Piano
      ( onMusic
      , releaseAllKeys
      , addNote
      , removeNote
      ) where

import qualified Data.Map.Strict as Map

import           Imj.Music.Types

addNote :: NoteSpec -> PressedKeys -> (Int, PressedKeys)
addNote n (PressedKeys s) = (1, PressedKeys $ Map.insertWith (+) n 1 s)

removeNote :: NoteSpec -> PressedKeys -> (Int,PressedKeys)
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

-- | Returns the [Music] such that the piano plays no note.
releaseAllKeys :: PressedKeys -> [Music]
releaseAllKeys (PressedKeys m) =
  concatMap (\(n,i) -> replicate i (StopNote n)) $ Map.assocs m

onMusic :: Music -> PressedKeys -> (Int,PressedKeys) -- ^ returns the number of changed notes (added or removed)
onMusic n = case n of
  StartNote spec _ -> addNote spec
  StopNote spec -> removeNote spec
