{-# LANGUAGE DeriveGeneric #-}

module Imj.Music.Piano
      ( PianoState(..)
      , mkEmptyPiano
      , modPiano
      , silencePiano
      , addNote
      , removeNote
      ) where

import           Imj.Prelude

import           Data.Map.Strict(Map)
import qualified Data.Map.Strict as Map

import           Imj.Music.Types

-- | Represents the keys currently pressed. Note that the same key can be pressed
-- twice if the lower and upper keyboards overlapp.
data PianoState = PianoState !(Map NoteSpec Int)
  deriving(Generic, Show)
instance Binary PianoState
instance NFData PianoState

mkEmptyPiano :: PianoState
mkEmptyPiano = PianoState mempty

addNote :: NoteSpec -> PianoState -> (Int, PianoState)
addNote n (PianoState s) = (1, PianoState $ Map.insertWith (+) n 1 s)

removeNote :: NoteSpec -> PianoState -> (Int,PianoState)
removeNote n (PianoState s) =
  (maybe 0 (const 1) mayNote, PianoState m)
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
silencePiano :: PianoState -> [Music]
silencePiano (PianoState m) =
  concatMap (\(n,i) -> replicate i (StopNote n)) $ Map.assocs m

modPiano :: Music -> PianoState -> (Int,PianoState) -- ^ returns the number of changed notes (added or removed)
modPiano n = case n of
  StartNote spec _ -> addNote spec
  StopNote spec -> removeNote spec
