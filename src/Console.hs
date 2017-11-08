module Console ( ConsoleConfig(..)
               , configureConsoleFor ) where


import System.Console.ANSI( clearScreen
                          , hideCursor
                          , showCursor )
import System.IO( hSetBuffering
                , hSetEcho
                , BufferMode( NoBuffering, LineBuffering )
                , stdin
                , stdout )


--------------------------------------------------------------------------------
-- Pure
--------------------------------------------------------------------------------

data ConsoleConfig = Gaming | Editing


--------------------------------------------------------------------------------
-- IO
--------------------------------------------------------------------------------

configureConsoleFor :: ConsoleConfig -> IO ()
configureConsoleFor config = do
  hSetEcho stdin $ case config of
      Gaming  -> False
      Editing -> True
  hSetBuffering stdout $ case config of
      Gaming  -> NoBuffering
      Editing -> LineBuffering
  case config of
    Gaming  -> hideCursor >> clearScreen
    Editing -> showCursor  -- do not clearScreen, to retain a potential printed exception
