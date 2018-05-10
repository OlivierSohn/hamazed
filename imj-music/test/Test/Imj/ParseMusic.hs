{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Test.Imj.ParseMusic
          ( testParseMusic
          ) where

import           Imj.Music.Types
import           Imj.Music.Compose
import           Imj.Music.Play

testParseMusic :: IO ()
testParseMusic = do

  [notes|do|] `shouldBe`
    [Note $ NoteSpec Do noOctave]

  [notes|do ré|] `shouldBe`
    [Note $ NoteSpec Do noOctave
    ,Note $ NoteSpec Ré noOctave
    ]

  [notes|do ré
|] `shouldBe`
    [Note $ NoteSpec Do noOctave
    ,Note $ NoteSpec Ré noOctave
    ]

  [notes|do ré
 |] `shouldBe`
    [Note $ NoteSpec Do noOctave
    ,Note $ NoteSpec Ré noOctave
    ]

  [notes|
  do ré
  |] `shouldBe`
    [Note $ NoteSpec Do noOctave
    ,Note $ NoteSpec Ré noOctave
    ]

  [notes|do v ré|] `shouldBe`
    [Note $ NoteSpec Do noOctave
    ,Note $ NoteSpec Ré (noOctave-1)
    ]

  [notes|do vré|] `shouldBe`
    [Note $ NoteSpec Do noOctave
    ,Note $ NoteSpec Ré (noOctave-1)
    ]

  [notes|do ^ré|] `shouldBe`
    [Note $ NoteSpec Do noOctave
    ,Note $ NoteSpec Ré (noOctave+1)
    ]

  [notes|do ^^ré|] `shouldBe`
    [Note $ NoteSpec Do noOctave
    ,Note $ NoteSpec Ré (noOctave+2)
    ]

  [notes|do ^vré|] `shouldBe`
    [Note $ NoteSpec Do noOctave
    ,Note $ NoteSpec Ré noOctave
    ]

  [notes|do vvré|] `shouldBe`
    [Note $ NoteSpec Do noOctave
    ,Note $ NoteSpec Ré (noOctave-2)
    ]

  [notes|do v v ré|] `shouldBe`
    [Note $ NoteSpec Do noOctave
    ,Note $ NoteSpec Ré (noOctave-2)
    ]

  [notes|do . do|] `shouldBe`
    [Note $ NoteSpec Do noOctave
    ,Rest
    ,Note $ NoteSpec Do noOctave
    ]

  [notes|do - do|] `shouldBe`
    [Note $ NoteSpec Do noOctave
    ,Extend
    ,Note $ NoteSpec Do noOctave
    ]

  [notes|do - - do|] `shouldBe`
    [Note $ NoteSpec Do noOctave
    ,Extend
    ,Extend
    ,Note $ NoteSpec Do noOctave
    ]

  [notes|do - . do|] `shouldBe`
    [Note $ NoteSpec Do noOctave
    ,Extend
    ,Rest
    ,Note $ NoteSpec Do noOctave
    ]

  [notes|do . - - do|] `shouldBe`
    [Note $ NoteSpec Do noOctave
    ,Rest
    ,Extend
    ,Extend
    ,Note $ NoteSpec Do noOctave
    ]

  (allMusic [notes|do ré mi|]) `shouldBe`
    [ [StartNote (NoteSpec Do (Octave 6)) (MidiVelocity 1.0)]
    , [StopNote (NoteSpec Do (Octave 6)), StartNote (NoteSpec Ré (Octave 6)) (MidiVelocity 1.0)]
    , [StopNote (NoteSpec Ré (Octave 6)), StartNote (NoteSpec Mi (Octave 6)) (MidiVelocity 1.0)]
    , [StopNote (NoteSpec Mi (Octave 6))]
    ]

  (allMusic [notes|do - mi|]) `shouldBe`
    [ [StartNote (NoteSpec Do (Octave 6)) (MidiVelocity 1.0)]
    , []
    , [StopNote (NoteSpec Do (Octave 6)), StartNote (NoteSpec Mi (Octave 6)) (MidiVelocity 1.0)]
    , [StopNote (NoteSpec Mi (Octave 6))]
    ]

  (allMusic [notes|do - - mi|]) `shouldBe`
    [ [StartNote (NoteSpec Do (Octave 6)) (MidiVelocity 1.0)]
    , []
    , []
    , [StopNote (NoteSpec Do (Octave 6)), StartNote (NoteSpec Mi (Octave 6)) (MidiVelocity 1.0)]
    , [StopNote (NoteSpec Mi (Octave 6))]
    ]

  (allMusic [notes|do - . mi|]) `shouldBe`
    [ [StartNote (NoteSpec Do (Octave 6)) (MidiVelocity 1.0)]
    , []
    , [StopNote (NoteSpec Do (Octave 6))]
    , [StartNote (NoteSpec Mi (Octave 6)) (MidiVelocity 1.0)]
    , [StopNote (NoteSpec Mi (Octave 6))]
    ]

  (allMusic [notes|do . - mi|]) `shouldBe`
    [ [StartNote (NoteSpec Do (Octave 6)) (MidiVelocity 1.0)]
    , [StopNote (NoteSpec Do (Octave 6))]
    , []
    , [StartNote (NoteSpec Mi (Octave 6)) (MidiVelocity 1.0)]
    , [StopNote (NoteSpec Mi (Octave 6))]
    ]

  (allMusic [notes|do . . mi|]) `shouldBe`
    [ [StartNote (NoteSpec Do (Octave 6)) (MidiVelocity 1.0)]
    , [StopNote (NoteSpec Do (Octave 6))]
    , []
    , [StartNote (NoteSpec Mi (Octave 6)) (MidiVelocity 1.0)]
    , [StopNote (NoteSpec Mi (Octave 6))]
    ]

  (allMusic [notes|do . mi|]) `shouldBe`
    [ [StartNote (NoteSpec Do (Octave 6)) (MidiVelocity 1.0)]
    , [StopNote (NoteSpec Do (Octave 6))]
    , [StartNote (NoteSpec Mi (Octave 6)) (MidiVelocity 1.0)]
    , [StopNote (NoteSpec Mi (Octave 6))]
    ]

  (allMusic [notes|- do . mi|]) `shouldBe`
    [ []
    , [StartNote (NoteSpec Do (Octave 6)) (MidiVelocity 1.0)]
    , [StopNote (NoteSpec Do (Octave 6))]
    , [StartNote (NoteSpec Mi (Octave 6)) (MidiVelocity 1.0)]
    , [StopNote (NoteSpec Mi (Octave 6))]
    ]

  (allMusic [notes|. do . mi|]) `shouldBe`
    [ []
    , [StartNote (NoteSpec Do (Octave 6)) (MidiVelocity 1.0)]
    , [StopNote (NoteSpec Do (Octave 6))]
    , [StartNote (NoteSpec Mi (Octave 6)) (MidiVelocity 1.0)]
    , [StopNote (NoteSpec Mi (Octave 6))]
    ]


shouldBe :: (Show a, Eq a) => a -> a -> IO ()
shouldBe actual expected =
  if actual == expected
    then
      return ()
    else
      error $ "expected\n" ++ show expected ++ " but got\n" ++ show actual
