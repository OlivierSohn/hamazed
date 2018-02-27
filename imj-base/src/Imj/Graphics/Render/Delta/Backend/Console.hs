{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE InstanceSigs #-}

module Imj.Graphics.Render.Delta.Backend.Console
    ( ConsoleBackend
    , newConsoleBackend
    ) where

import           Imj.Prelude

import           GHC.IO.Encoding(setLocaleEncoding)

import           Control.Concurrent(forkIO)
import           Control.Concurrent.STM(TQueue, newTQueueIO, atomically, writeTQueue)
import           Data.Vector.Unboxed.Mutable(read)
import qualified System.Console.Terminal.Size as Terminal(Window(..), size)
import           System.Console.ANSI(clearScreen, hideCursor
                                   , setSGR, setCursorPosition, showCursor)
import           System.IO(hSetBuffering, hGetBuffering, hSetEcho, hFlush
                         , BufferMode(..), stdin, stdout, utf8)

import           Imj.Data.StdoutBuffer(Stdout, mkStdoutBuffer, addStr, addChar, flush)
import qualified Imj.Data.Vector.Unboxed.Mutable.Dynamic as Dyn
                        (IOVector, unstableSort, accessUnderlying, length)
import           Imj.Geo.Discrete.Types
import           Imj.Graphics.Color.Types
import           Imj.Graphics.Render.Delta.Env
import           Imj.Graphics.Render.Delta.Types
import           Imj.Graphics.Render.Delta.Internal.Types
import           Imj.Graphics.Render.Delta.Cell
import           Imj.Input.Blocking
import           Imj.Input.Types


data ConsoleBackend = ConsoleBackend !(TQueue Key) !Stdout

instance DeltaRenderBackend ConsoleBackend where
  render (ConsoleBackend _ buf) a b =
    liftIO $ deltaRenderConsole buf a b
  cleanup _ =
    liftIO $ configureConsoleFor Editing LineBuffering
  cycleRenderingOption _ =
    return ()
  getDiscreteSize _ =
    liftIO $
      maybe
        Nothing
        (\(Terminal.Window h w) ->
            Just $ Size (fromIntegral h) (fromIntegral w))
        <$> (Terminal.size :: IO (Maybe (Terminal.Window Int)))

instance PlayerInput ConsoleBackend where
  programShouldEnd _ = return False
  keysQueue (ConsoleBackend q _) = q
  queueType _ = AutomaticFeed
  pollKeys _ = return ()
  waitKeysTimeout _ _ = return ()
  {-# INLINABLE programShouldEnd #-}
  {-# INLINABLE keysQueue #-}
  {-# INLINABLE queueType #-}
  {-# INLINABLE pollKeys #-}
  {-# INLINABLE waitKeysTimeout #-}

newConsoleBackend :: IO ConsoleBackend
newConsoleBackend = do
  setLocaleEncoding utf8 -- because 'Stdout' encodes using utf8
  configureConsoleFor Gaming defaultStdoutMode
  newTQueueIO >>= \q -> do
    _ <- forkIO $ forever $ getKeyThenFlush >>= atomically . writeTQueue q
    ConsoleBackend q <$> mkStdoutBuffer 8192

-- | @=@ 'BlockBuffering' $ 'Just' 'maxBound'
defaultStdoutMode :: BufferMode
defaultStdoutMode =
  BlockBuffering $ Just maxBound -- maximize the buffer size to avoid screen tearing

data ConsoleConfig = Gaming | Editing

configureConsoleFor :: ConsoleConfig -> BufferMode -> IO ()
configureConsoleFor config stdoutMode =
  hSetBuffering stdout stdoutMode >>
  case config of
    Gaming  -> do
      hSetEcho stdin False
      hideCursor
      clearScreen -- do not clearFromCursorToScreenEnd with 0 0, so as to keep
                  -- the current console content above the game.
      let requiredInputBuffering = NoBuffering
      initialIb <- hGetBuffering stdin
      hSetBuffering stdin requiredInputBuffering
      ib <- hGetBuffering stdin
      when (ib /= requiredInputBuffering) $
         error $ "input buffering mode "
               ++ show initialIb
               ++ " could not be changed to "
               ++ show requiredInputBuffering
               ++ " instead it is now "
               ++ show ib
    Editing -> do
      hSetEcho stdin True
      showCursor
      -- do not clearFromCursorToScreenEnd, to retain a potential printed exception
      setSGR []
      Terminal.size
        >>= maybe
              (return ())
              (\(Terminal.Window x _) -> setCursorPosition (pred x) 0)
      hSetBuffering stdout LineBuffering

deltaRenderConsole :: Stdout -> Delta -> Dim Width -> IO (Time Duration System, Time Duration System)
deltaRenderConsole b (Delta delta) w = do
  t1 <- getSystemTime
  -- On average, foreground and background color change command is 20 bytes :
  --   "\ESC[48;5;167;38;5;255m"
  -- On average, position change command is 9 bytes :
  --   "\ESC[150;42H"
  -- So we want to minimize the number of color changes first, and then mimnimize
  -- the number of position changes.
  -- In 'Cell', color is encoded in higher bits than position, so this sort
  -- sorts by color first, then by position, which is what we want.
  Dyn.unstableSort delta
  renderDelta delta w b
  t2 <- getSystemTime
  flush b
  liftIO $ hFlush stdout -- TODO is flush blocking? slow? could it be async?
  t3 <- getSystemTime
  return (t1...t2, t2...t3)

renderDelta :: Dyn.IOVector Cell
            -> Dim Width
            -> Stdout
            -> IO ()
renderDelta delta' w b = do
  sz <- Dyn.length delta'
  delta <- Dyn.accessUnderlying delta'
      -- We pass the underlying vector, and the size instead of the dynamicVector
  let go :: Dim BufferIndex
         -> Maybe LayeredColor
         -> Maybe (Dim BufferIndex)
         -> IO LayeredColor
      go index prevColors prevIndex
       | fromIntegral sz == index =
          return whiteOnBlack -- this value is not used
       | otherwise = do
          c <- read delta $ fromIntegral index
          let (bg, fg, idx, char) = expandIndexed c
              prevRendered = (== Just (pred idx)) prevIndex
          setCursorPositionIfNeeded w idx prevRendered b
          usedColor <- renderCell bg fg char prevColors b
          go (succ index) (Just usedColor) (Just idx)
  void $ go 0 Nothing Nothing

-- | The command to set the cursor position to 123,45 is "\ESC[123;45H",
-- its size is 9 bytes : one order of magnitude more than the size
-- of a char, so we avoid sending this command when not strictly needed.
{-# INLINE setCursorPositionIfNeeded #-}
setCursorPositionIfNeeded :: Dim Width
                          -> Dim BufferIndex
                          -- ^ the buffer index
                          -> Bool
                          -- ^ True if a char was rendered at the previous buffer index
                          -> Stdout
                          -> IO ()
setCursorPositionIfNeeded w idx predPosRendered b = do
  let (colIdx, rowIdx) = xyFromIndex w idx
      shouldSetCursorPosition =
      -- We assume that the buffer width is not equal to terminal width,
      -- so even if the previous position was rendered,
      -- the cursor may not be located at the beginning of the line.
        colIdx == 0
      -- If the previous buffer position was rendered, the cursor position has
      -- automatically advanced to the next column (or to the beginning of
      -- the next line if it was the last terminal column).
        || not predPosRendered
  when shouldSetCursorPosition
    $ addStr (setCursorPositionCode (fromIntegral rowIdx) (fromIntegral colIdx)) b

{-# INLINE setCursorPositionCode #-}
setCursorPositionCode :: Int -- ^ 0-based row to move to
                      -> Int -- ^ 0-based column to move to
                      -> String
setCursorPositionCode n m = csi [n + 1, m + 1] "H"

{-# INLINE renderCell #-}
renderCell :: Color8 Background
           -> Color8 Foreground
           -> Char
           -> Maybe LayeredColor
           -> Stdout
           -> IO LayeredColor
renderCell bg fg char maybeCurrentConsoleColor b = do
  let (bgChange, fgChange, usedFg) =
        maybe
          (True, True, fg)
          (\(LayeredColor bg' fg') ->
              -- use foreground color if we don't draw a space
              let useFg = char /= ' ' -- I don't use Data.Char.isSpace, it could be slower
                  usedFg' = if useFg
                             then
                               fg
                             else
                               fg'
              in (bg'/=bg, fg'/=usedFg', usedFg'))
            maybeCurrentConsoleColor
      sgrs = concat $ [color8FgSGRToCode fg | fgChange] ++
                      [color8BgSGRToCode bg | bgChange]

  if bgChange || fgChange
    then
      addStr (csi sgrs "m" ++ [char]) b
    else
      addChar char b
  return $ LayeredColor bg usedFg

-- TODO use this formalism
{-
newtype SetPosition = Move2d !Int !Int
                    | Move1 Direction !Int

type Value = (Color8 Background, Color8 Foreground, Char)
type Location = (Row, Column)

screenLocations = { (row, column) | row <- [0..screenHeight], column <- [0..screenWidth] }

type Step = Int  -- represents a temporal game / animation step

frame :: Step -> Location -> Value  -- defines the desired content of animations

identicalLocations n = {loc | loc <- screenLocations && frame n loc == frame (pred n) loc}
deltaLocations     n = screenLocations \\ (identicalLocations n)

newtype RenderCmd   = SetPosition | SetColor | Char | !String
newtype SetPosition = Move2d Int Int | Move Direction Int
newtype SetColor    = SetColorForeground | SetColorBackground | SetColorBoth
newtype Direction   = Up | Left | Down | Right

cheapestChangePosition' :: (Row,Col) -> SetPosition
cheapestChangePosition :: (Row,Col) -> (Row,Col) -> Maybe SetPosition

cheapestChangeColor' :: (Background Color, Foreground Color) -> SetColor
cheapestChangeColor :: (Background Color, Foreground Color) -> (Background Color, Foreground Color) -> Maybe SetColor

cost :: RenderCmd -> Int -- the cost is in bytes

render :: [(Location,Value)] -> IO ()
render l = do
  let cmds = cheapestCmds l
      str = concatMap asString cmds
  printStr str
  hFlush stdout

cheapestCmds :: [(Location,Value)] -> [RenderCmd]
cheapestCmds [] = []
cheapestCmds l =
  let l' = sortDelta l
  in renderFirst (head l') ++ cheapestCmds' l'

cheapestCmds' :: [(Location,Value)] -> [RenderCmd]
cheapestCmds' [] = error ""
cheapestCmds' [a] = []
cheapestCmds' l@(a:b:_) = renderNext a b ++ cheapestCmds' (tail l)

sortDelta :: [(Location,Value)] -> [(Location,Value)]
sortDelta = sortByColorThenIncreasingLocation

renderFirst :: (Location, Value) -> RenderCmd
renderNext :: (Location,Value) -> (Location,Value) -> RenderCmd -- Choses the best command (the cheapest one) when there are multiple possibilities.
-}
