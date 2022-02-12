{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}

module Test.Imj.ReadMidi
          ( testReadMidi
          ) where

import           Data.Text(unpack)

import           Imj.Audio.Output
import           Imj.Music.Instrument
import           Imj.Music.Midi

testReadMidi :: IO ()
testReadMidi = do
  -- run this test only if we can use audio.
  usingAudioOutput (return ()) >>= either
    (\e -> putStrLn $ "skipping test, no audio output is available :" ++ show (unpack e))
    (\_ -> do
      let --part = "./midi/HappyBirthday.mid"
          --part = "midi/liszt_hungarian_fantasia_for_orchestra_(c)laviano.mid" -- contains very low basses
          part = "midi/tchaikovsky_swan_lake_10_(c)lucarelli (1).mid"
      -- verify usingAudioOutput is reentrant
      usingAudioOutput (usingAudioOutput $ usingAudioOutput $
        playMidiFile part $ trapezoidalInstrument 401)
          >>= \case
        (Right (Right (Right (Right an)))) ->
          hasFinishedConsistently an `shouldBe` Right ()
        _ -> error ""

      -- verify successive initialization / deinitialization is ok
      usingAudioOutput(
        playMidiFile part $ trapezoidalInstrument 5000)
          >>= \case
        (Right (Right an)) ->
          hasFinishedConsistently an `shouldBe` Right ()
        _ -> error ""
      )


shouldBe :: (Show a, Eq a) => a -> a -> IO ()
shouldBe actual expected =
  if actual == expected
    then
      return ()
    else
      error $ "expected\n" ++ show expected ++ " but got\n" ++ show actual
