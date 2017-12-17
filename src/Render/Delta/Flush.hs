{-# OPTIONS_HADDOCK hide #-}

module Render.Delta.Flush
    ( deltaFlush
    ) where

import           Prelude hiding(read, length)

import           Control.Monad(when)
import           Data.IORef( IORef , readIORef )
import           Data.Vector.Unboxed.Mutable( IOVector, read, write, length )
import           System.IO( stdout, hFlush )
import           System.Console.ANSI( Color8Code(..)
                                    , setCursorPosition, setSGRCode, SGR(..), ConsoleLayer(..) )

import           Color
import           Render.Delta.Buffers
import           Render.Delta.Cell
import           Render.Delta.Clear
import           Render.Delta.Internal.Types
import qualified Render.Delta.DynUnboxedVec as Dyn
                                (unstableSort, accessUnderlying, length,
                                 clear, pushBack )
import           Render.Delta.Types


-- | Flushes the frame, i.e renders it to the console.
--   Then, resizes the context if needed (see 'ResizePolicy')
--   and clears the back buffer (see 'ClearPolicy').
deltaFlush :: IORef Buffers -> IO ()
deltaFlush ioRefBuffers =
  readIORef ioRefBuffers
    >>=
      render
        >> do
          updateSize ioRefBuffers
          -- TODO if buffers resized because the terminal resized, send a clearScreen command or re-render with new size
          hFlush stdout -- TODO is flush blocking? slow? could it be async?


render :: Buffers -> IO ()
render buffers@(Buffers _ _ width (Delta delta) _) = do
  computeDelta buffers 0

  clearIfNeeded OnFrame buffers

  -- On average, foreground and background color change command is 20 bytes :
  --   "\ESC[48;5;167;38;5;255m"
  -- On average, position change command is 9 bytes :
  --   "\ESC[150;42H"
  -- So we want to minimize the number of color changes first, and then mimnimize
  -- the number of position changes.
  -- In 'Cell', color is encoded in higher bits than position, so this sort
  -- sorts by color first, then by position, which is what we want.
  Dyn.unstableSort delta

  szDelta <- Dyn.length delta
  under <- Dyn.accessUnderlying delta
  -- We ignore this color value. We could store it and use it to initiate the recursion
  -- at next render but if the client renders with another library in-betweeen, this value
  -- would be wrong, so we can ignore it here for more robustness.
  _ <- renderDelta under (fromIntegral szDelta) width 0 Nothing Nothing
  Dyn.clear delta

-- We pass the underlying vector, and the size instead of the dynamicVector
renderDelta :: IOVector Cell
            -> Dim Size
            -> Dim Width
            -> Dim Index
            -> Maybe LayeredColor
            -> Maybe (Dim Index)
            -> IO LayeredColor
renderDelta delta size width index prevColors prevIndex
 | fromIntegral size == index = return $ LayeredColor (Color8Code 0) (Color8Code 0)
 | otherwise = do
    c <- read delta $ fromIntegral index
    let (bg, fg, idx, char) = expandIndexed c
        prevRendered = (== Just (pred idx)) prevIndex
    setCursorPositionIfNeeded width idx prevRendered
    usedColor <- renderCell bg fg char prevColors
    renderDelta delta size width (succ index) (Just usedColor) (Just idx)


computeDelta :: Buffers
             -> Dim Index
             -- ^ the buffer index
             -> IO ()
computeDelta
 b@(Buffers (Buffer backBuf) (Buffer frontBuf) _ (Delta delta) _)
 idx
  | fromIntegral idx == size = return ()
  | otherwise = do
      let i = fromIntegral idx
      -- read from back buffer
      valueToDisplay <- read backBuf i
      -- read from front buffer
      valueCurrentlyDisplayed <- read frontBuf i
      -- if differences are found, update front buffer and push the difference
      -- in delta vector
      when (valueToDisplay /= valueCurrentlyDisplayed) $ do
          write frontBuf i valueToDisplay
          Dyn.pushBack delta $ mkIndexedCell valueToDisplay idx
      -- recurse
      computeDelta b (succ idx)
  where
    size = length backBuf

-- | The command to set the cursor position to 123,45 is "\ESC[123;45H",
-- its size is 9 bytes : one order of magnitude more than the size
-- of a char, so we avoid sending this command when not strictly needed.
{-# INLINE setCursorPositionIfNeeded #-}
setCursorPositionIfNeeded :: Dim Width
                          -> Dim Index
                          -- ^ the buffer index
                          -> Bool
                          -- ^ True if a char was rendered at the previous buffer index
                          -> IO ()
setCursorPositionIfNeeded width idx predPosRendered = do
  let (colIdx, rowIdx) = xyFromIndex width idx
      shouldSetCursorPosition =
      -- We assume that the buffer width is not equal to terminal width,
      -- so even if the previous position was rendered,
      -- the cursor may not be located at the beginning of the line.
        colIdx == 0
      -- If the previous buffer position was rendered, the cursor position has
      -- automatically advanced to the next column (or to the beginning of
      -- the next line if it was the last terminal column).
        || not predPosRendered
  when shouldSetCursorPosition $ setCursorPosition (fromIntegral rowIdx) (fromIntegral colIdx)

{-# INLINE renderCell #-}
renderCell :: Color8Code -> Color8Code -> Char -> Maybe LayeredColor -> IO LayeredColor
renderCell bg fg char maybeCurrentConsoleColor = do
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
      sgrs = [SetPaletteColor Foreground fg | fgChange] ++
             [SetPaletteColor Background bg | bgChange]

  if bgChange || fgChange
    then
      Prelude.putStr $ setSGRCode sgrs ++ [char]
    else
      Prelude.putChar char
  return $ LayeredColor bg usedFg


{-# INLINE xyFromIndex #-}
xyFromIndex :: Dim Width -> Dim Index -> (Dim ColIndex, Dim RowIndex)
xyFromIndex width idx =
  getRowCol idx width


-- TODO use this formalism
{-
type Value = (Background Color, Foreground Color, Char)
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
