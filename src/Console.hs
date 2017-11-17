module Console ( ConsoleConfig(..)
               , configureConsoleFor
               -- rendering functions
               , beginFrame
               , endFrame
               , setForeground
               , renderChar_
               , renderStrLn
               , renderStrLn_
               , RenderState(..)
               , renderSegment
               -- reexport System.Console.ANSI
               , ColorIntensity(..)
               , Color(..)
               ) where

import           Imajuscule.Prelude


import           System.Console.ANSI( clearScreen
                                    , hideCursor
                                    , setSGR
                                    , showCursor
                                    , SGR(..)
                                    , ConsoleLayer(..)
                                    , ColorIntensity(..)
                                    , Color(..) )
import           System.IO( hSetBuffering
                          , hSetEcho
                          , BufferMode( .. )
                          , stdin
                          , stdout )


import           Geo( Col(..)
                    , Coords(..)
                    , Segment(..)
                    , sumCoords
                    , translateInDir
                    , Direction(..)
                    , Row(..) )

--{--
import qualified RenderBackend.Classic as Backend(
--}
{--
import qualified RenderBackend.DoubleBuffering as Backend(
--}
                                                           beginFrame
                                                         , endFrame
                                                         , moveTo
                                                         , renderChar
                                                         , renderStr
                                                         , setForeground )

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
  -- TODO the backends should handle this
  hSetBuffering stdout $ case config of
      Gaming  -> BlockBuffering Nothing -- use a big buffer, which we will flush
                                        -- after each game update
      Editing -> LineBuffering
  case config of
    Gaming  -> do
      hideCursor
      clearScreen
    Editing -> do
      showCursor
      -- do not clearScreen, to retain a potential printed exception
      setSGR [SetColor Foreground Vivid White]

beginFrame :: IO ()
beginFrame = Backend.beginFrame

endFrame :: IO ()
endFrame = Backend.endFrame

setForeground :: ColorIntensity -> Color -> IO ()
setForeground = Backend.setForeground

renderChar_ :: Char -> RenderState -> IO ()
renderChar_ char (RenderState c) = do
  Backend.moveTo c
  Backend.renderChar char
  return ()


renderStrLn_ :: String -> RenderState -> IO ()
renderStrLn_ str (RenderState c) = do
  Backend.moveTo c
  Backend.renderStr str

renderStrLn :: String -> RenderState -> IO RenderState
renderStrLn str (RenderState c) = do
  Backend.moveTo c
  Backend.renderStr str
  return $ RenderState $ translateInDir Down c


renderSegment :: Segment -> Char -> RenderState -> IO ()
renderSegment l = case l of
  Horizontal row c1 c2 -> renderHorizontalLine row c1 c2
  Vertical col r1 r2   -> renderVerticalLine   col r1 r2
  Oblique _ _ -> error "oblique segment rendering is not supported"


renderVerticalLine :: Col -> Int -> Int -> Char -> RenderState -> IO ()
renderVerticalLine col r1 r2 char (RenderState upperLeft) = do
  let rows = [(min r1 r2)..(max r1 r2)]
  mapM_ (renderChar_ char . (\r -> RenderState $ sumCoords upperLeft $ Coords (Row r) col)) rows

renderHorizontalLine :: Row -> Int -> Int -> Char -> RenderState -> IO ()
renderHorizontalLine row c1 c2 char (RenderState upperLeft) =
  renderStrLn_ (replicate (1 + abs (c2-c1)) char) $ RenderState $ sumCoords upperLeft $ Coords row (Col (min c1 c2))
