{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Test.Imj.ParseMusic
          ( testParseMusic
          ) where

import           Imj.Music.Types
import           Imj.Music.Compose
import           Imj.Music.Instruments
import           Imj.Music.Play

testParseMusic :: IO ()
testParseMusic = do
  let i = bellInstrument

  testParseMusicWithComments

  [notes|do|] `shouldBe`
    [Note Do noOctave]

  [notes|do ré|] `shouldBe`
    [Note Do noOctave
    ,Note Ré noOctave
    ]

  [notes|do ré
|] `shouldBe`
    [Note Do noOctave
    ,Note Ré noOctave
    ]

  [notes|do ré
 |] `shouldBe`
    [Note Do noOctave
    ,Note Ré noOctave
    ]

  [notes|
  do ré
  |] `shouldBe`
    [Note Do noOctave
    ,Note Ré noOctave
    ]

  [notes|do v ré|] `shouldBe`
    [Note Do noOctave
    ,Note Ré (noOctave-1)
    ]

  [notes|do vré|] `shouldBe`
    [Note Do noOctave
    ,Note Ré (noOctave-1)
    ]

  [notes|do ^ré|] `shouldBe`
    [Note Do noOctave
    ,Note Ré (noOctave+1)
    ]

  [notes|do ^^ré|] `shouldBe`
    [Note Do noOctave
    ,Note Ré (noOctave+2)
    ]

  [notes|do ^vré|] `shouldBe`
    [Note Do noOctave
    ,Note Ré noOctave
    ]

  [notes|do vvré|] `shouldBe`
    [Note Do noOctave
    ,Note Ré (noOctave-2)
    ]

  [notes|do v v ré|] `shouldBe`
    [Note Do noOctave
    ,Note Ré (noOctave-2)
    ]

  [notes|do . do|] `shouldBe`
    [Note Do noOctave
    ,Rest
    ,Note Do noOctave
    ]

  [notes|do - do|] `shouldBe`
    [Note Do noOctave
    ,Extend
    ,Note Do noOctave
    ]

  [notes|do - - do|] `shouldBe`
    [Note Do noOctave
    ,Extend
    ,Extend
    ,Note Do noOctave
    ]

  [notes|do - . do|] `shouldBe`
    [Note Do noOctave
    ,Extend
    ,Rest
    ,Note Do noOctave
    ]

  [notes|do . - - do|] `shouldBe`
    [Note Do noOctave
    ,Rest
    ,Extend
    ,Extend
    ,Note Do noOctave
    ]

  (allMusic i [notes|do ré mi|]) `shouldBe`
    [ [StartNote (NoteSpec Do (Octave 6) i) (MidiVelocity 1.0)]
    , [StopNote (NoteSpec Do (Octave 6) i), StartNote (NoteSpec Ré (Octave 6) i) (MidiVelocity 1.0)]
    , [StopNote (NoteSpec Ré (Octave 6) i), StartNote (NoteSpec Mi (Octave 6) i) (MidiVelocity 1.0)]
    , [StopNote (NoteSpec Mi (Octave 6) i)]
    ]

  (allMusic i [notes|do - mi|]) `shouldBe`
    [ [StartNote (NoteSpec Do (Octave 6) i) (MidiVelocity 1.0)]
    , []
    , [StopNote (NoteSpec Do (Octave 6) i), StartNote (NoteSpec Mi (Octave 6) i) (MidiVelocity 1.0)]
    , [StopNote (NoteSpec Mi (Octave 6) i)]
    ]

  (allMusic i [notes|do - - mi|]) `shouldBe`
    [ [StartNote (NoteSpec Do (Octave 6) i) (MidiVelocity 1.0)]
    , []
    , []
    , [StopNote (NoteSpec Do (Octave 6) i), StartNote (NoteSpec Mi (Octave 6) i) (MidiVelocity 1.0)]
    , [StopNote (NoteSpec Mi (Octave 6) i)]
    ]

  (allMusic i [notes|do - . mi|]) `shouldBe`
    [ [StartNote (NoteSpec Do (Octave 6) i) (MidiVelocity 1.0)]
    , []
    , [StopNote (NoteSpec Do (Octave 6) i)]
    , [StartNote (NoteSpec Mi (Octave 6) i) (MidiVelocity 1.0)]
    , [StopNote (NoteSpec Mi (Octave 6) i)]
    ]

  (allMusic i [notes|do . - mi|]) `shouldBe`
    [ [StartNote (NoteSpec Do (Octave 6) i) (MidiVelocity 1.0)]
    , [StopNote (NoteSpec Do (Octave 6) i)]
    , []
    , [StartNote (NoteSpec Mi (Octave 6) i) (MidiVelocity 1.0)]
    , [StopNote (NoteSpec Mi (Octave 6) i)]
    ]

  (allMusic i [notes|do . . mi|]) `shouldBe`
    [ [StartNote (NoteSpec Do (Octave 6) i) (MidiVelocity 1.0)]
    , [StopNote (NoteSpec Do (Octave 6) i)]
    , []
    , [StartNote (NoteSpec Mi (Octave 6) i) (MidiVelocity 1.0)]
    , [StopNote (NoteSpec Mi (Octave 6) i)]
    ]

  (allMusic i [notes|do . mi|]) `shouldBe`
    [ [StartNote (NoteSpec Do (Octave 6) i) (MidiVelocity 1.0)]
    , [StopNote (NoteSpec Do (Octave 6) i)]
    , [StartNote (NoteSpec Mi (Octave 6) i) (MidiVelocity 1.0)]
    , [StopNote (NoteSpec Mi (Octave 6) i)]
    ]

  (allMusic i [notes|- do . mi|]) `shouldBe`
    [ []
    , [StartNote (NoteSpec Do (Octave 6) i) (MidiVelocity 1.0)]
    , [StopNote (NoteSpec Do (Octave 6) i)]
    , [StartNote (NoteSpec Mi (Octave 6) i) (MidiVelocity 1.0)]
    , [StopNote (NoteSpec Mi (Octave 6) i)]
    ]

  (allMusic i [notes|. do . mi|]) `shouldBe`
    [ []
    , [StartNote (NoteSpec Do (Octave 6) i) (MidiVelocity 1.0)]
    , [StopNote (NoteSpec Do (Octave 6) i)]
    , [StartNote (NoteSpec Mi (Octave 6) i) (MidiVelocity 1.0)]
    , [StopNote (NoteSpec Mi (Octave 6) i)]
    ]

testParseMusicWithComments :: IO ()
testParseMusicWithComments = do
  let expected =
        [ Note Do noOctave
        , Note Ré noOctave
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
