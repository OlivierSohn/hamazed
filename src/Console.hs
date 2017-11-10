module Console ( ConsoleConfig(..)
               , configureConsoleFor
               , renderStrLn
               , RenderState(..)
               , renderSegment ) where

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
                    , Segment(..)
                    , sumCoords
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


renderStrLn :: String -> RenderState -> IO RenderState
renderStrLn str (RenderState (Coords (Row r) (Col c))) = do
  setCursorPosition r c
  Prelude.putStrLn str
  return $ RenderState $ Coords (Row $ r + 1) (Col c)


renderSegment :: Segment -> Char -> RenderState -> IO RenderState
renderSegment l = case l of
  Horizontal row c1 c2 -> renderHorizontalLine row c1 c2
  Vertical col r1 r2   -> renderVerticalLine   col r1 r2
  Oblique _ _ -> error "oblique segment rendering is not supported"


renderVerticalLine :: Col -> Int -> Int -> Char -> RenderState -> IO RenderState
renderVerticalLine col r1 r2 char rs@(RenderState upperLeft) = do
  let rows = [(min r1 r2)..(max r1 r2)]
  res <- mapM (renderStrLn [char] . (\r -> RenderState $ sumCoords upperLeft $ Coords (Row r) col)) rows
  return $ case res of
    [] ->  rs
    x:_ -> x

renderHorizontalLine :: Row -> Int -> Int -> Char -> RenderState -> IO RenderState
renderHorizontalLine row c1 c2 char (RenderState upperLeft) =
  renderStrLn (replicate (1 + abs (c2-c1)) char) $ RenderState $ sumCoords upperLeft $ Coords row (Col (min c1 c2))
