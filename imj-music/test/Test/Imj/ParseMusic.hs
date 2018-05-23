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
  let i = defaultInstrument

  testParseMusicWithComments

  [notes|do|] `shouldBe`
    [Note $ NoteSpec Do noOctave i]

  [notes|do ré|] `shouldBe`
    [Note $ NoteSpec Do noOctave i
    ,Note $ NoteSpec Ré noOctave i
    ]

  [notes|do ré
|] `shouldBe`
    [Note $ NoteSpec Do noOctave i
    ,Note $ NoteSpec Ré noOctave i
    ]

  [notes|do ré
 |] `shouldBe`
    [Note $ NoteSpec Do noOctave i
    ,Note $ NoteSpec Ré noOctave i
    ]

  [notes|
  do ré
  |] `shouldBe`
    [Note $ NoteSpec Do noOctave i
    ,Note $ NoteSpec Ré noOctave i
    ]

  [notes|do v ré|] `shouldBe`
    [Note $ NoteSpec Do noOctave i
    ,Note $ NoteSpec Ré (noOctave-1) i
    ]

  [notes|do vré|] `shouldBe`
    [Note $ NoteSpec Do noOctave i
    ,Note $ NoteSpec Ré (noOctave-1) i
    ]

  [notes|do ^ré|] `shouldBe`
    [Note $ NoteSpec Do noOctave i
    ,Note $ NoteSpec Ré (noOctave+1) i
    ]

  [notes|do ^^ré|] `shouldBe`
    [Note $ NoteSpec Do noOctave i
    ,Note $ NoteSpec Ré (noOctave+2) i
    ]

  [notes|do ^vré|] `shouldBe`
    [Note $ NoteSpec Do noOctave i
    ,Note $ NoteSpec Ré noOctave i
    ]

  [notes|do vvré|] `shouldBe`
    [Note $ NoteSpec Do noOctave i
    ,Note $ NoteSpec Ré (noOctave-2) i
    ]

  [notes|do v v ré|] `shouldBe`
    [Note $ NoteSpec Do noOctave i
    ,Note $ NoteSpec Ré (noOctave-2) i
    ]

  [notes|do . do|] `shouldBe`
    [Note $ NoteSpec Do noOctave i
    ,Rest
    ,Note $ NoteSpec Do noOctave i
    ]

  [notes|do - do|] `shouldBe`
    [Note $ NoteSpec Do noOctave i
    ,Extend
    ,Note $ NoteSpec Do noOctave i
    ]

  [notes|do - - do|] `shouldBe`
    [Note $ NoteSpec Do noOctave i
    ,Extend
    ,Extend
    ,Note $ NoteSpec Do noOctave i
    ]

  [notes|do - . do|] `shouldBe`
    [Note $ NoteSpec Do noOctave i
    ,Extend
    ,Rest
    ,Note $ NoteSpec Do noOctave i
    ]

  [notes|do . - - do|] `shouldBe`
    [Note $ NoteSpec Do noOctave i
    ,Rest
    ,Extend
    ,Extend
    ,Note $ NoteSpec Do noOctave i
    ]

  (allMusic [notes|do ré mi|]) `shouldBe`
    [ [StartNote (NoteSpec Do (Octave 6) i) (MidiVelocity 1.0)]
    , [StopNote (NoteSpec Do (Octave 6) i), StartNote (NoteSpec Ré (Octave 6) i) (MidiVelocity 1.0)]
    , [StopNote (NoteSpec Ré (Octave 6) i), StartNote (NoteSpec Mi (Octave 6) i) (MidiVelocity 1.0)]
    , [StopNote (NoteSpec Mi (Octave 6) i)]
    ]

  (allMusic [notes|do - mi|]) `shouldBe`
    [ [StartNote (NoteSpec Do (Octave 6) i) (MidiVelocity 1.0)]
    , []
    , [StopNote (NoteSpec Do (Octave 6) i), StartNote (NoteSpec Mi (Octave 6) i) (MidiVelocity 1.0)]
    , [StopNote (NoteSpec Mi (Octave 6) i)]
    ]

  (allMusic [notes|do - - mi|]) `shouldBe`
    [ [StartNote (NoteSpec Do (Octave 6) i) (MidiVelocity 1.0)]
    , []
    , []
    , [StopNote (NoteSpec Do (Octave 6) i), StartNote (NoteSpec Mi (Octave 6) i) (MidiVelocity 1.0)]
    , [StopNote (NoteSpec Mi (Octave 6) i)]
    ]

  (allMusic [notes|do - . mi|]) `shouldBe`
    [ [StartNote (NoteSpec Do (Octave 6) i) (MidiVelocity 1.0)]
    , []
    , [StopNote (NoteSpec Do (Octave 6) i)]
    , [StartNote (NoteSpec Mi (Octave 6) i) (MidiVelocity 1.0)]
    , [StopNote (NoteSpec Mi (Octave 6) i)]
    ]

  (allMusic [notes|do . - mi|]) `shouldBe`
    [ [StartNote (NoteSpec Do (Octave 6) i) (MidiVelocity 1.0)]
    , [StopNote (NoteSpec Do (Octave 6) i)]
    , []
    , [StartNote (NoteSpec Mi (Octave 6) i) (MidiVelocity 1.0)]
    , [StopNote (NoteSpec Mi (Octave 6) i)]
    ]

  (allMusic [notes|do . . mi|]) `shouldBe`
    [ [StartNote (NoteSpec Do (Octave 6) i) (MidiVelocity 1.0)]
    , [StopNote (NoteSpec Do (Octave 6) i)]
    , []
    , [StartNote (NoteSpec Mi (Octave 6) i) (MidiVelocity 1.0)]
    , [StopNote (NoteSpec Mi (Octave 6) i)]
    ]

  (allMusic [notes|do . mi|]) `shouldBe`
    [ [StartNote (NoteSpec Do (Octave 6) i) (MidiVelocity 1.0)]
    , [StopNote (NoteSpec Do (Octave 6) i)]
    , [StartNote (NoteSpec Mi (Octave 6) i) (MidiVelocity 1.0)]
    , [StopNote (NoteSpec Mi (Octave 6) i)]
    ]

  (allMusic [notes|- do . mi|]) `shouldBe`
    [ []
    , [StartNote (NoteSpec Do (Octave 6) i) (MidiVelocity 1.0)]
    , [StopNote (NoteSpec Do (Octave 6) i)]
    , [StartNote (NoteSpec Mi (Octave 6) i) (MidiVelocity 1.0)]
    , [StopNote (NoteSpec Mi (Octave 6) i)]
    ]

  (allMusic [notes|. do . mi|]) `shouldBe`
    [ []
    , [StartNote (NoteSpec Do (Octave 6) i) (MidiVelocity 1.0)]
    , [StopNote (NoteSpec Do (Octave 6) i)]
    , [StartNote (NoteSpec Mi (Octave 6) i) (MidiVelocity 1.0)]
    , [StopNote (NoteSpec Mi (Octave 6) i)]
    ]

testParseMusicWithComments :: IO ()
testParseMusicWithComments = do
  let expected =
        fmap (\f -> f defaultInstrument)
          [Note . NoteSpec Do noOctave
          ,Note . NoteSpec Ré noOctave
          ]
  [notes|do:
    ré|]
    `shouldBe` expected

  [notes|do:
    ré:|]
    `shouldBe` expected

  [notes|do:comment1
    ré:comment2|]
    `shouldBe` expected

  [notes|do   :  comment1  : with inner colons :: :: !
    ré  :  comment2  : with inner colons :: :: !|]
    `shouldBe` expected

  [notes|do   :  comment1  : with inner colons :: :: !
              :  a comment on an empty line1
              :  a comment on an empty line2
         ré   :  comment2  : with inner colons :: :: !
              :  a comment on an empty line3
              :  a comment on an empty line4
    |]
    `shouldBe` expected

  [notes|     :  commentOnTheFirstLine
              :  a comment on an empty line0
         do   :  comment1  : with inner colons :: :: !
              :  a comment on an empty line1
              :  a comment on an empty line2
         ré   :  comment2  : with inner colons :: :: !
              :  a comment on an empty line3
              :  a comment on an empty line4
    |]
    `shouldBe` expected

shouldBe :: (Show a, Eq a) => a -> a -> IO ()
shouldBe actual expected =
  if actual == expected
    then
      return ()
    else
      error $ "expected\n" ++ show expected ++ " but got\n" ++ show actual
