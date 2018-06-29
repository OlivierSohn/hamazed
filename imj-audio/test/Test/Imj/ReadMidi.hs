{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Imj.ReadMidi
          ( testReadMidi
          ) where

import           Data.Text(unpack)

import           Imj.Music.Midi
import           Imj.Audio.Output

testReadMidi :: IO ()
testReadMidi = do
  -- run this test only if we can use audio.
  usingAudioOutput (return ()) >>= either
    (\e -> putStrLn $ "skipping test, no audio output is available :" ++ show (unpack e))
    (\_ -> do
      -- verify usingAudioOutput is reentrant
      res <- usingAudioOutput $ usingAudioOutput $ usingAudioOutput $ playMidiFile "./midi/HappyBirthday.mid"
        --"midi/liszt_hungarian_fantasia_for_orchestra_(c)laviano.mid"
        --"midi/tchaikovsky_swan_lake_10_(c)lucarelli (1).mid"


      res `shouldBe` (Right (Right (Right (Right ()))))

      -- verify successive initialization / deinitialization is ok
      res2 <- usingAudioOutput $ playMidiFile "./midi/HappyBirthday.mid"
        --"./midi/tchaikovsky_swan_lake_10_(c)lucarelli (1).mid"

      res2 `shouldBe` (Right (Right ()))
      return ()
      )


shouldBe :: (Show a, Eq a) => a -> a -> IO ()
shouldBe actual expected =
  if actual == expected
    then
      return ()
    else
      error $ "expected\n" ++ show expected ++ " but got\n" ++ show actual
