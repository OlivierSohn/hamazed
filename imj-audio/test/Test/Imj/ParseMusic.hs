{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Imj.ParseMusic
          ( testParseMonoVoice
          , testParsePolyVoice
          ) where

import           Imj.Audio

testParseMonoVoice :: IO ()
testParseMonoVoice = do
  testParseMonoVoiceWithComments

  [voice||] `shouldBe`
    ([] :: [VoiceInstruction])

  [voice|do|] `shouldBe`
    [Note Do noOctave]

{-
  We could use this notation to allow multine melodies within a block:

  [voice|
  | do ré mi
  | sol do ré
  mi fa sol : upper line voice, continued
  | la sol mi

  | fa sol sol : the previous blank line indicates a different system
    |] `shouldBe`
    [Note Do noOctave]
-}

  [voice|do ré|] `shouldBe`
    [Note Do noOctave
    ,Note Ré noOctave
    ]

  [voice|do ré
|] `shouldBe`
    [Note Do noOctave
    ,Note Ré noOctave
    ]

  [voice|do ré
 |] `shouldBe`
    [Note Do noOctave
    ,Note Ré noOctave
    ]

  [voice|
  do ré
  |] `shouldBe`
    [Note Do noOctave
    ,Note Ré noOctave
    ]

  [voice|do v ré|] `shouldBe`
    [Note Do noOctave
    ,Note Ré (noOctave-1)
    ]

  [voice|do vré|] `shouldBe`
    [Note Do noOctave
    ,Note Ré (noOctave-1)
    ]

  [voice|do ^ré|] `shouldBe`
    [Note Do noOctave
    ,Note Ré (noOctave+1)
    ]

  [voice|do ^^ré|] `shouldBe`
    [Note Do noOctave
    ,Note Ré (noOctave+2)
    ]

  [voice|do ^vré|] `shouldBe`
    [Note Do noOctave
    ,Note Ré noOctave
    ]

  [voice|do vvré|] `shouldBe`
    [Note Do noOctave
    ,Note Ré (noOctave-2)
    ]

  [voice|do v v ré|] `shouldBe`
    [Note Do noOctave
    ,Note Ré (noOctave-2)
    ]

  [voice|do . do|] `shouldBe`
    [Note Do noOctave
    ,Rest
    ,Note Do noOctave
    ]

  [voice|do - do|] `shouldBe`
    [Note Do noOctave
    ,Extend
    ,Note Do noOctave
    ]

  [voice|do - - do|] `shouldBe`
    [Note Do noOctave
    ,Extend
    ,Extend
    ,Note Do noOctave
    ]

  [voice|do - . do|] `shouldBe`
    [Note Do noOctave
    ,Extend
    ,Rest
    ,Note Do noOctave
    ]

  [voice|do . - - do|] `shouldBe`
    [Note Do noOctave
    ,Rest
    ,Extend
    ,Extend
    ,Note Do noOctave
    ]

  let i = bellInstrument

  (allMusic i [voice|do ré mi|]) `shouldBe`
    [ [StartNote (InstrumentNote Do (Octave 6) i) (NoteVelocity 1.0)]
    , [StopNote (InstrumentNote Do (Octave 6) i), StartNote (InstrumentNote Ré (Octave 6) i) (NoteVelocity 1.0)]
    , [StopNote (InstrumentNote Ré (Octave 6) i), StartNote (InstrumentNote Mi (Octave 6) i) (NoteVelocity 1.0)]
    , [StopNote (InstrumentNote Mi (Octave 6) i)]
    ]

  (allMusic i [voice|do - mi|]) `shouldBe`
    [ [StartNote (InstrumentNote Do (Octave 6) i) (NoteVelocity 1.0)]
    , []
    , [StopNote (InstrumentNote Do (Octave 6) i), StartNote (InstrumentNote Mi (Octave 6) i) (NoteVelocity 1.0)]
    , [StopNote (InstrumentNote Mi (Octave 6) i)]
    ]

  (allMusic i [voice|do - - mi|]) `shouldBe`
    [ [StartNote (InstrumentNote Do (Octave 6) i) (NoteVelocity 1.0)]
    , []
    , []
    , [StopNote (InstrumentNote Do (Octave 6) i), StartNote (InstrumentNote Mi (Octave 6) i) (NoteVelocity 1.0)]
    , [StopNote (InstrumentNote Mi (Octave 6) i)]
    ]

  (allMusic i [voice|do - . mi|]) `shouldBe`
    [ [StartNote (InstrumentNote Do (Octave 6) i) (NoteVelocity 1.0)]
    , []
    , [StopNote (InstrumentNote Do (Octave 6) i)]
    , [StartNote (InstrumentNote Mi (Octave 6) i) (NoteVelocity 1.0)]
    , [StopNote (InstrumentNote Mi (Octave 6) i)]
    ]

  (allMusic i [voice|do . - mi|]) `shouldBe`
    [ [StartNote (InstrumentNote Do (Octave 6) i) (NoteVelocity 1.0)]
    , [StopNote (InstrumentNote Do (Octave 6) i)]
    , []
    , [StartNote (InstrumentNote Mi (Octave 6) i) (NoteVelocity 1.0)]
    , [StopNote (InstrumentNote Mi (Octave 6) i)]
    ]

  (allMusic i [voice|do . . mi|]) `shouldBe`
    [ [StartNote (InstrumentNote Do (Octave 6) i) (NoteVelocity 1.0)]
    , [StopNote (InstrumentNote Do (Octave 6) i)]
    , []
    , [StartNote (InstrumentNote Mi (Octave 6) i) (NoteVelocity 1.0)]
    , [StopNote (InstrumentNote Mi (Octave 6) i)]
    ]

  (allMusic i [voice|do . mi|]) `shouldBe`
    [ [StartNote (InstrumentNote Do (Octave 6) i) (NoteVelocity 1.0)]
    , [StopNote (InstrumentNote Do (Octave 6) i)]
    , [StartNote (InstrumentNote Mi (Octave 6) i) (NoteVelocity 1.0)]
    , [StopNote (InstrumentNote Mi (Octave 6) i)]
    ]

  (allMusic i [voice|- do . mi|]) `shouldBe`
    [ []
    , [StartNote (InstrumentNote Do (Octave 6) i) (NoteVelocity 1.0)]
    , [StopNote (InstrumentNote Do (Octave 6) i)]
    , [StartNote (InstrumentNote Mi (Octave 6) i) (NoteVelocity 1.0)]
    , [StopNote (InstrumentNote Mi (Octave 6) i)]
    ]

  (allMusic i [voice|. do . mi|]) `shouldBe`
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
  [voice|do:
    ré|]
    `shouldBe` expected

  [voice|do:
    ré:|]
    `shouldBe` expected

  [voice|do:comment1
    ré:comment2|]
    `shouldBe` expected

  [voice|do   :  comment1  : with inner colons :: :: !
    ré  :  comment2  : with inner colons :: :: !|]
    `shouldBe` expected

  [voice|do   :  comment1  : with inner colons :: :: !
              :  a comment on an empty line1
              :  a comment on an empty line2
         ré   :  comment2  : with inner colons :: :: !
              :  a comment on an empty line3
              :  a comment on an empty line4
    |]
    `shouldBe` expected

  [voice|     :  commentOnTheFirstLine
              :  a comment on an empty line0
         do   :  comment1  : with inner colons :: :: !
              :  a comment on an empty line1
              :  a comment on an empty line2
         ré   :  comment2  : with inner colons :: :: !
              :  a comment on an empty line3
              :  a comment on an empty line4
    |]
    `shouldBe` expected


testParsePolyVoice :: IO ()
testParsePolyVoice = do

  [voices||] `shouldBe`
    ([] :: [[VoiceInstruction]])

  [voices|do|] `shouldBe`
    [[Note Do noOctave]]

  [voices|do
    |] `shouldBe`
    [[Note Do noOctave]]

  [voices|
  do|] `shouldBe`
    [[Note Do noOctave]]

  [voices|

  do

    |] `shouldBe`
    [[Note Do noOctave]]

  [voices|
  do ré|] `shouldBe`
    [[Note Do noOctave, Note Ré noOctave]]

  [voices|
  do
  ré|] `shouldBe`
    [[Note Do noOctave]
    ,[Note Ré noOctave]]

  -- using a blank line as separator
  [voices|
  do

  ré|] `shouldBe`
    [[Note Do noOctave, Note Ré noOctave]]

  [voices|
  do



  ré|] `shouldBe`
    [[Note Do noOctave, Note Ré noOctave]]

  [voices|



  do



  ré


  |] `shouldBe`
    [[Note Do noOctave, Note Ré noOctave]]

  -- pad lines that are shorter
  [voices|
  do mi
  ré|] `shouldBe`
    [[Note Do noOctave, Note Mi noOctave]
    ,[Note Ré noOctave, Rest]]

  [voices|
  do mi sol
  ré|] `shouldBe`
    [[Note Do noOctave, Note Mi noOctave, Note Sol noOctave]
    ,[Note Ré noOctave, Rest, Rest]]

  [voices|
  do mi
  ré

  fa sol la|] `shouldBe`
    [[Note Do noOctave, Note Mi noOctave, Note Fa noOctave, Note Sol noOctave, Note La noOctave]
    ,[Note Ré noOctave, Rest, Rest, Rest, Rest]]

  [voices|
  fa sol la

  do mi
  ré|] `shouldBe`
    [[Note Fa noOctave, Note Sol noOctave, Note La noOctave, Note Do noOctave, Note Mi noOctave]
    ,[Rest, Rest, Rest, Note Ré noOctave, Rest]]

  [voices|

  : This describes the piece.

  fa sol la

  do mi
  ré|] `shouldBe`
    [[Note Fa noOctave, Note Sol noOctave, Note La noOctave, Note Do noOctave, Note Mi noOctave]
    ,[Rest, Rest, Rest, Note Ré noOctave, Rest]]

  [voices|
  fa sol la : comment1

  do mi
  ré|] `shouldBe`
    [[Note Fa noOctave, Note Sol noOctave, Note La noOctave, Note Do noOctave, Note Mi noOctave]
    ,[Rest, Rest, Rest, Note Ré noOctave, Rest]]

  [voices|
  fa sol la
  : comment1

  do mi
  ré|] `shouldBe`
    [[Note Fa noOctave, Note Sol noOctave, Note La noOctave, Note Do noOctave, Note Mi noOctave]
    ,[Rest, Rest, Rest, Note Ré noOctave, Rest]]

shouldBe :: (Show a, Eq a) => a -> a -> IO ()
shouldBe actual expected =
  if actual == expected
    then
      return ()
    else
      error $ "expected\n" ++ show expected ++ " but got\n" ++ show actual
