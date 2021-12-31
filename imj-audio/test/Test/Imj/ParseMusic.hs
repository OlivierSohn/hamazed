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
    ([] :: [Instruction])

  [voice|do|] `shouldBe`
    [Note Do noOctave]

{-
  We could use this notation to allow multine melodies within a block:

  [voice|
  | do re mi
  | sol do re
  mi fa sol : upper line voice, continued
  | la sol mi

  | fa sol sol : the previous blank line indicates a different system
    |] `shouldBe`
    [Note Do noOctave]
-}

  [voice|do re|] `shouldBe`
    [Note Do noOctave
    ,Note Re noOctave
    ]

  [voice|do re
|] `shouldBe`
    [Note Do noOctave
    ,Note Re noOctave
    ]

  [voice|do re
 |] `shouldBe`
    [Note Do noOctave
    ,Note Re noOctave
    ]

  [voice|
  do re
  |] `shouldBe`
    [Note Do noOctave
    ,Note Re noOctave
    ]

  [voice|do v re|] `shouldBe`
    [Note Do noOctave
    ,Note Re (noOctave-1)
    ]

  [voice|do vre|] `shouldBe`
    [Note Do noOctave
    ,Note Re (noOctave-1)
    ]

  [voice|do ^re|] `shouldBe`
    [Note Do noOctave
    ,Note Re (noOctave+1)
    ]

  [voice|do ^^re|] `shouldBe`
    [Note Do noOctave
    ,Note Re (noOctave+2)
    ]

  [voice|do ^vre|] `shouldBe`
    [Note Do noOctave
    ,Note Re noOctave
    ]

  [voice|do vvre|] `shouldBe`
    [Note Do noOctave
    ,Note Re (noOctave-2)
    ]

  [voice|do v v re|] `shouldBe`
    [Note Do noOctave
    ,Note Re (noOctave-2)
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

  (allMusic i panCentered [voice|do re mi|]) `shouldBe`
    [ [StartNote Nothing (InstrumentNote Do (Octave 6) i) (NoteVelocity 1.0) panCentered]
    , [StopNote Nothing (InstrumentNote Do (Octave 6) i), StartNote Nothing (InstrumentNote Re (Octave 6) i) (NoteVelocity 1.0) panCentered]
    , [StopNote Nothing (InstrumentNote Re (Octave 6) i), StartNote Nothing (InstrumentNote Mi (Octave 6) i) (NoteVelocity 1.0) panCentered]
    , [StopNote Nothing (InstrumentNote Mi (Octave 6) i)]
    ]

  (allMusic i panCentered [voice|do - mi|]) `shouldBe`
    [ [StartNote Nothing (InstrumentNote Do (Octave 6) i) (NoteVelocity 1.0) panCentered]
    , []
    , [StopNote Nothing (InstrumentNote Do (Octave 6) i), StartNote Nothing (InstrumentNote Mi (Octave 6) i) (NoteVelocity 1.0) panCentered]
    , [StopNote Nothing (InstrumentNote Mi (Octave 6) i)]
    ]

  (allMusic i panCentered [voice|do - - mi|]) `shouldBe`
    [ [StartNote Nothing (InstrumentNote Do (Octave 6) i) (NoteVelocity 1.0) panCentered]
    , []
    , []
    , [StopNote Nothing (InstrumentNote Do (Octave 6) i), StartNote Nothing (InstrumentNote Mi (Octave 6) i) (NoteVelocity 1.0) panCentered]
    , [StopNote Nothing (InstrumentNote Mi (Octave 6) i)]
    ]

  (allMusic i panCentered [voice|do - . mi|]) `shouldBe`
    [ [StartNote Nothing (InstrumentNote Do (Octave 6) i) (NoteVelocity 1.0) panCentered]
    , []
    , [StopNote Nothing (InstrumentNote Do (Octave 6) i)]
    , [StartNote Nothing (InstrumentNote Mi (Octave 6) i) (NoteVelocity 1.0) panCentered]
    , [StopNote Nothing (InstrumentNote Mi (Octave 6) i)]
    ]

  (allMusic i panCentered [voice|do . - mi|]) `shouldBe`
    [ [StartNote Nothing (InstrumentNote Do (Octave 6) i) (NoteVelocity 1.0) panCentered]
    , [StopNote Nothing (InstrumentNote Do (Octave 6) i)]
    , []
    , [StartNote Nothing (InstrumentNote Mi (Octave 6) i) (NoteVelocity 1.0) panCentered]
    , [StopNote Nothing (InstrumentNote Mi (Octave 6) i)]
    ]

  (allMusic i panCentered [voice|do . . mi|]) `shouldBe`
    [ [StartNote Nothing (InstrumentNote Do (Octave 6) i) (NoteVelocity 1.0) panCentered]
    , [StopNote Nothing (InstrumentNote Do (Octave 6) i)]
    , []
    , [StartNote Nothing (InstrumentNote Mi (Octave 6) i) (NoteVelocity 1.0) panCentered]
    , [StopNote Nothing (InstrumentNote Mi (Octave 6) i)]
    ]

  (allMusic i panCentered [voice|do . mi|]) `shouldBe`
    [ [StartNote Nothing (InstrumentNote Do (Octave 6) i) (NoteVelocity 1.0) panCentered]
    , [StopNote Nothing (InstrumentNote Do (Octave 6) i)]
    , [StartNote Nothing (InstrumentNote Mi (Octave 6) i) (NoteVelocity 1.0) panCentered]
    , [StopNote Nothing (InstrumentNote Mi (Octave 6) i)]
    ]

  (allMusic i panCentered [voice|- do . mi|]) `shouldBe`
    [ []
    , [StartNote Nothing (InstrumentNote Do (Octave 6) i) (NoteVelocity 1.0) panCentered]
    , [StopNote Nothing (InstrumentNote Do (Octave 6) i)]
    , [StartNote Nothing (InstrumentNote Mi (Octave 6) i) (NoteVelocity 1.0) panCentered]
    , [StopNote Nothing (InstrumentNote Mi (Octave 6) i)]
    ]

  (allMusic i panCentered [voice|. do . mi|]) `shouldBe`
    [ []
    , [StartNote Nothing (InstrumentNote Do (Octave 6) i) (NoteVelocity 1.0) panCentered]
    , [StopNote Nothing (InstrumentNote Do (Octave 6) i)]
    , [StartNote Nothing (InstrumentNote Mi (Octave 6) i) (NoteVelocity 1.0) panCentered]
    , [StopNote Nothing (InstrumentNote Mi (Octave 6) i)]
    ]

testParseMonoVoiceWithComments :: IO ()
testParseMonoVoiceWithComments = do
  let expected =
        [ Note Do noOctave
        , Note Re noOctave
        ]
  [voice|do:
    re|]
    `shouldBe` expected

  [voice|do:
    re:|]
    `shouldBe` expected

  [voice|do:comment1
    re:comment2|]
    `shouldBe` expected

  [voice|do   :  comment1  : with inner colons :: :: !
    re  :  comment2  : with inner colons :: :: !|]
    `shouldBe` expected

  [voice|do   :  comment1  : with inner colons :: :: !
              :  a comment on an empty line1
              :  a comment on an empty line2
         re   :  comment2  : with inner colons :: :: !
              :  a comment on an empty line3
              :  a comment on an empty line4
    |]
    `shouldBe` expected

  [voice|     :  commentOnTheFirstLine
              :  a comment on an empty line0
         do   :  comment1  : with inner colons :: :: !
              :  a comment on an empty line1
              :  a comment on an empty line2
         re   :  comment2  : with inner colons :: :: !
              :  a comment on an empty line3
              :  a comment on an empty line4
    |]
    `shouldBe` expected


testParsePolyVoice :: IO ()
testParsePolyVoice = do

  [voices||] `shouldBe`
    ([] :: [[Instruction]])

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
  do re|] `shouldBe`
    [[Note Do noOctave, Note Re noOctave]]

  [voices|
  do
  re|] `shouldBe`
    [[Note Do noOctave]
    ,[Note Re noOctave]]


  [voices|
  do
  : a commented line will not act as a separator
  re|] `shouldBe`
    [[Note Do noOctave]
    ,[Note Re noOctave]]

  -- using a blank line as separator
  [voices|
  do

  re|] `shouldBe`
    [[Note Do noOctave, Note Re noOctave]]

  [voices|
  do



  re|] `shouldBe`
    [[Note Do noOctave, Note Re noOctave]]

  [voices|



  do



  re


  |] `shouldBe`
    [[Note Do noOctave, Note Re noOctave]]

  -- pad lines that are shorter
  [voices|
  do mi
  re|] `shouldBe`
    [[Note Do noOctave, Note Mi noOctave]
    ,[Note Re noOctave, Rest]]

  [voices|
  do mi sol
  re|] `shouldBe`
    [[Note Do noOctave, Note Mi noOctave, Note Sol noOctave]
    ,[Note Re noOctave, Rest, Rest]]

  [voices|
  do mi
  re

  fa sol la|] `shouldBe`
    [[Note Do noOctave, Note Mi noOctave, Note Fa noOctave, Note Sol noOctave, Note La noOctave]
    ,[Note Re noOctave, Rest, Rest, Rest, Rest]]

  [voices|
  fa sol la

  do mi
  re|] `shouldBe`
    [[Note Fa noOctave, Note Sol noOctave, Note La noOctave, Note Do noOctave, Note Mi noOctave]
    ,[Rest, Rest, Rest, Note Re noOctave, Rest]]

  [voices|

  : This describes the piece.

  fa sol la

  do mi
  re|] `shouldBe`
    [[Note Fa noOctave, Note Sol noOctave, Note La noOctave, Note Do noOctave, Note Mi noOctave]
    ,[Rest, Rest, Rest, Note Re noOctave, Rest]]

  [voices|
  fa sol la : comment1

  do mi
  re|] `shouldBe`
    [[Note Fa noOctave, Note Sol noOctave, Note La noOctave, Note Do noOctave, Note Mi noOctave]
    ,[Rest, Rest, Rest, Note Re noOctave, Rest]]

  [voices|
  fa sol la
  : comment1

  do mi
  re|] `shouldBe`
    [[Note Fa noOctave, Note Sol noOctave, Note La noOctave, Note Do noOctave, Note Mi noOctave]
    ,[Rest, Rest, Rest, Note Re noOctave, Rest]]

shouldBe :: (Show a, Eq a) => a -> a -> IO ()
shouldBe actual expected =
  if actual == expected
    then
      return ()
    else
      error $ "expected\n" ++ show expected ++ " but got\n" ++ show actual
