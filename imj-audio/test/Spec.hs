import Test.Imj.ParseMusic
import Test.Imj.ReadMidi

main :: IO ()
main = do
  testParseMonoVoice
  testParsePolyVoice
  testReadMidi
