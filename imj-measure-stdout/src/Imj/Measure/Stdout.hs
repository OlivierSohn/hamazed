
module Imj.Measure.Stdout
    ( testStdout
    , testStdoutSize -- for its doc
    , Settings(..)
    -- * Reexports
    ) where

import           Imj.Prelude

import           Control.Concurrent
import           System.IO


data Settings = LengthTest {
  _settingsLengthTestLength :: !Int
}
               | OtherTests !Bool


cursorForward, cursorBackward, noop:: String
cursorForward = "\ESC[C"
cursorBackward = "\ESC[D"
noop = cursorForward ++ cursorBackward

-- | Creates a string of given length, whith one visible character,
-- a very big number of no-op escape sequences (cursor forward, cursor backward)
-- and spaces to pad the string to the desired length.
mkSmartStringToExactlyFillBufferOfSize :: Char
                                       -- ^ the char that will be visible
                                       -> Int
                                       -- ^ the length of the string
                                       -> String
mkSmartStringToExactlyFillBufferOfSize char bufferSz =
  let noopSz = length noop
      bufferSzMinusCharSz = pred bufferSz
      countNoops = quot bufferSzMinusCharSz noopSz
      remainingSize = bufferSzMinusCharSz - countNoops * noopSz
      str = replicate remainingSize ' ' ++ [char] ++ concat (replicate countNoops noop)
  in if length str /= bufferSz
       then
         error $ "test logic : " ++ show (str,bufferSz)
       else
         str

{- | A function to visually see if the stdout size is smaller or bigger than given numbers.

@
if you see 1, then 2, then 3,
    size of stdout buffer < size
if you see 1, then 2 and 3 at the same time,
    size of stdout buffer >= size
@

see 'testStdoutSize' doc to understand how to determine the size.
-}
testStdoutSizes :: Int -> IO ()
testStdoutSizes size = do

  when (size <= 1) $ error $ "The size (" ++ show size ++ ") should be >= 2."

  print "test stdout size :"
  print " -if you see 1, then 2, then 3:"
  print $ "    maximum size of stdout buffer < " ++ show size
  print " -if you see 1, then 2 and 3 at the same time:"
  print $ "    maximum size of stdout buffer >= " ++ show size
  -- on my system, 8192 is the stdout max buffer size for
  -- BlockBuffering and LineBuffering modes
  mapM_ testStdoutSize [size]
  putStrLn ""

{- | This function, when run from a process whose stdout is the console,
can be used to visually determine the maximum capacity of stdout.

If stdout capacity >= sz, then in the console we will see

> 1

then

> 1  2  3

Else we see

> 1

then

> 1  2

then

> 1  2  3

There can be "0 to length 'noop'" spaces between chars, they are used
to pad the string sent to the buffer.
-}
testStdoutSize :: Int -> IO ()
testStdoutSize sz = do
  withBufferMode $ BlockBuffering $ Just sz

  let str = mkSmartStringToExactlyFillBufferOfSize '1' sz
      delay = 1000000
  putStr str
  threadDelay delay

  let str' = mkSmartStringToExactlyFillBufferOfSize '2' (pred sz)
  putStr str'
  threadDelay delay

  putChar '3'
  hFlush stdout
  threadDelay delay

withBufferMode :: BufferMode -> IO ()
withBufferMode b = do
  setBufferModeAndVerify b
  bufmode <- hShow stdout
  putStrLn ""
  putStrLn $ "Buffer Mode = " ++ bufmode
  putStrLn ""
  hFlush stdout


-- TODO ask the user wha to do : testStdoutSizes (and which size) or other.
-- maybe pass using command line params.
testStdout :: Settings -> IO ()
testStdout (LengthTest candidateLength) = testStdoutSizes candidateLength
testStdout (OtherTests False) = error "missing option (use --help to see the different possibilities)"
testStdout (OtherTests True) = do
  let actions = [
                -- shows that the 'n' in "BlockBuffering (Just n)" is homogenous to
                -- a string length, containing both displayable characters and characters
                -- of escape sequences:
                -- for "BlockBuffering (Just 4)", each flush renders
                --     | cursorforward
                -- for "BlockBuffering (Just 5)", flush renders in alternance:
                --          | cursorforward |
                --          cursorforward | cursorForward
                -- It also shows that the \0 for the end of the string is not stored
                -- in the buffer (ie the length of the buffer used for cursorforward is 3)
                 (putChar '|' >> putStr cursorForward, "putChar '|' >> putStr cursorForward")

                -- shows the effect of buffering on displayable characters
                ,(putChar '|', "putChar '|'")

                -- shows the effect of explicit flushes
                ,(putChar '|' >> hFlush stdout, "putChar '|' >> hFlush stdout")
                ]
  mapM_ (uncurry testAction) actions

testAction :: IO () -> String -> IO ()
testAction action desc = do
  print $ "-- " ++ desc
  let runTests = runActionTests action
      modes =
        [ -- NoBuffering -- it seems equivalent to "BlockBuffering (Just 1)"
          BlockBuffering Nothing
        , BlockBuffering (Just 1)
        , BlockBuffering (Just 2)
        , BlockBuffering (Just 4)
        , BlockBuffering (Just 5)
        , BlockBuffering (Just 8)
        , BlockBuffering (Just 16)
        , LineBuffering
        ]

  mapM_ runTests modes

runActionTests :: IO () -> BufferMode -> IO ()
runActionTests action b = do
  withBufferMode b

  let tenthSecond = 400000
  every tenthSecond action 16

every :: Int -> IO () -> Int -> IO ()
every period action remaining
  | remaining == 0 = return ()
  | otherwise = do
      action
      threadDelay period
      every period action (pred remaining)

setBufferModeAndVerify :: BufferMode -> IO ()
setBufferModeAndVerify b = do
  hSetBuffering stdout b
  b' <- hGetBuffering stdout
  when (b /= b') $ error $ "Failed to set buffering: " ++ show (b,b')
  return ()
