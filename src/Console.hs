module Console ( ConsoleConfig(..)
               , configureConsoleFor
               , renderStrLn
               , RenderState(..) ) where

import           System.Console.ANSI( clearScreen
                                    , hideCursor
                                    , setCursorPosition
                                    , showCursor )
import           System.IO( hSetBuffering
                          , hSetEcho
                          , BufferMode( .. )
                          , stdin
                          , stdout )


import           Geo( Col(..)
                    , Coords(..)
                    , Row(..) )



--------------------------------------------------------------------------------
-- Pure
--------------------------------------------------------------------------------

data ConsoleConfig = Gaming | Editing

newtype RenderState = RenderState {
    _currentUpperLeftCorner :: Coords
}

--------------------------------------------------------------------------------
-- IO
--------------------------------------------------------------------------------

configureConsoleFor :: ConsoleConfig -> IO ()
configureConsoleFor config = do
  hSetEcho stdin $ case config of
      Gaming  -> False
      Editing -> True
  hSetBuffering stdout $ case config of
      Gaming  -> BlockBuffering Nothing -- use a big buffer, which we will flush
                                        -- after each game update
      Editing -> LineBuffering
  case config of
    Gaming  -> hideCursor >> clearScreen
    Editing -> showCursor  -- do not clearScreen, to retain a potential printed exception


renderStrLn :: RenderState -> String -> IO RenderState
renderStrLn (RenderState (Coords (Row r) (Col c))) str = do
  setCursorPosition r c
  Prelude.putStrLn str
  return $ RenderState $ Coords (Row $ r + 1) (Col c)
