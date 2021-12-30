import Test.Imj.ParseMusic
--import Test.Imj.ReadMidi
import Test.Imj.Harmony

main :: IO ()
main = do
  testParseMonoVoice
  testParsePolyVoice
  testDescList
  --testReadMidi
