{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Test.Imj.ParseMusic
          ( testParseMonoVoice
          ) where

import           Imj.Music.Types
import           Imj.Music.Compose
import           Imj.Music.Instruments
import           Imj.Music.Play

testParseMonoVoice :: IO ()
testParseMonoVoice = do
  let i = bellInstrument

  testParseMonoVoiceWithComments

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
    [ [StartNote (InstrumentNote Do (Octave 6) i) (NoteVelocity 1.0)]
    , [StopNote (InstrumentNote Do (Octave 6) i), StartNote (InstrumentNote Ré (Octave 6) i) (NoteVelocity 1.0)]
    , [StopNote (InstrumentNote Ré (Octave 6) i), StartNote (InstrumentNote Mi (Octave 6) i) (NoteVelocity 1.0)]
    , [StopNote (InstrumentNote Mi (Octave 6) i)]
    ]

  (allMusic i [notes|do - mi|]) `shouldBe`
    [ [StartNote (InstrumentNote Do (Octave 6) i) (NoteVelocity 1.0)]
    , []
    , [StopNote (InstrumentNote Do (Octave 6) i), StartNote (InstrumentNote Mi (Octave 6) i) (NoteVelocity 1.0)]
    , [StopNote (InstrumentNote Mi (Octave 6) i)]
    ]

  (allMusic i [notes|do - - mi|]) `shouldBe`
    [ [StartNote (InstrumentNote Do (Octave 6) i) (NoteVelocity 1.0)]
    , []
    , []
    , [StopNote (InstrumentNote Do (Octave 6) i), StartNote (InstrumentNote Mi (Octave 6) i) (NoteVelocity 1.0)]
    , [StopNote (InstrumentNote Mi (Octave 6) i)]
    ]

  (allMusic i [notes|do - . mi|]) `shouldBe`
    [ [StartNote (InstrumentNote Do (Octave 6) i) (NoteVelocity 1.0)]
    , []
    , [StopNote (InstrumentNote Do (Octave 6) i)]
    , [StartNote (InstrumentNote Mi (Octave 6) i) (NoteVelocity 1.0)]
    , [StopNote (InstrumentNote Mi (Octave 6) i)]
    ]

  (allMusic i [notes|do . - mi|]) `shouldBe`
    [ [StartNote (InstrumentNote Do (Octave 6) i) (NoteVelocity 1.0)]
    , [StopNote (InstrumentNote Do (Octave 6) i)]
    , []
    , [StartNote (InstrumentNote Mi (Octave 6) i) (NoteVelocity 1.0)]
    , [StopNote (InstrumentNote Mi (Octave 6) i)]
    ]

  (allMusic i [notes|do . . mi|]) `shouldBe`
    [ [StartNote (InstrumentNote Do (Octave 6) i) (NoteVelocity 1.0)]
    , [StopNote (InstrumentNote Do (Octave 6) i)]
    , []
    , [StartNote (InstrumentNote Mi (Octave 6) i) (NoteVelocity 1.0)]
    , [StopNote (InstrumentNote Mi (Octave 6) i)]
    ]

  (allMusic i [notes|do . mi|]) `shouldBe`
    [ [StartNote (InstrumentNote Do (Octave 6) i) (NoteVelocity 1.0)]
    , [StopNote (InstrumentNote Do (Octave 6) i)]
    , [StartNote (InstrumentNote Mi (Octave 6) i) (NoteVelocity 1.0)]
    , [StopNote (InstrumentNote Mi (Octave 6) i)]
    ]

  (allMusic i [notes|- do . mi|]) `shouldBe`
    [ []
    , [StartNote (InstrumentNote Do (Octave 6) i) (NoteVelocity 1.0)]
    , [StopNote (InstrumentNote Do (Octave 6) i)]
    , [StartNote (InstrumentNote Mi (Octave 6) i) (NoteVelocity 1.0)]
    , [StopNote (InstrumentNote Mi (Octave 6) i)]
    ]

  (allMusic i [notes|. do . mi|]) `shouldBe`
    [ []
    , [StartNote (InstrumentNote Do (Octave 6) i) (NoteVelocity 1.0)]
    , [StopNote (InstrumentNote Do (Octave 6) i)]
    , [StartNote (InstrumentNote Mi (Octave 6) i) (NoteVelocity 1.0)]
    , [StopNote (InstrumentNote Mi (Octave 6) i)]
    ]

testParseMonoVoiceWithComments :: IO ()
testParseMonoVoiceWithComments = do
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

{- TODO
testParsePolyVoice :: IO ()
testParsePolyVoice = do
  let i = bellInstrument

  testParseMonoVoiceWithComments

  [poly|do|] `shouldBe`
    [[Note Do noOctave]]
-}

shouldBe :: (Show a, Eq a) => a -> a -> IO ()
shouldBe actual expected =
  if actual == expected
    then
      return ()
    else
      error $ "expected\n" ++ show expected ++ " but got\n" ++ show actual
