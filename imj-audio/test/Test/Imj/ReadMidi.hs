{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Imj.ReadMidi
          ( testReadMidi
          ) where

import           Imj.Music.Midi
import           Imj.Audio.Output

testReadMidi :: IO ()
testReadMidi = do
  -- verify usingAudioOutput is reentrant
  res <- usingAudioOutput $ usingAudioOutput $ usingAudioOutput $ playMidiFile
    --"midi/liszt_hungarian_fantasia_for_orchestra_(c)laviano.mid"
    --"midi/tchaikovsky_swan_lake_10_(c)lucarelli (1).mid"
    "./midi/HappyBirthday.mid"

  res `shouldBe` (Right ())

  -- verify successive initialization / deinitialization is ok
  res2 <- usingAudioOutput $ playMidiFile
    --"./midi/tchaikovsky_swan_lake_10_(c)lucarelli (1).mid"
    "./midi/HappyBirthday.mid"

  res2 `shouldBe` (Right ())

shouldBe :: (Show a, Eq a) => a -> a -> IO ()
shouldBe actual expected =
  if actual == expected
    then
      return ()
    else
      error $ "expected\n" ++ show expected ++ " but got\n" ++ show actual
