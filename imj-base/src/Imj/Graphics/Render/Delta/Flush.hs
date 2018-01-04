{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Graphics.Render.Delta.Flush
    ( deltaFlush
    ) where

import           Imj.Prelude

import qualified Prelude(putStr, putChar)

import           Control.Monad(when)
import           Data.IORef( IORef , readIORef, writeIORef)
import           Data.Vector.Unboxed.Mutable( IOVector, read, write, length )
import           System.IO( stdout, hFlush )

import qualified Imj.Data.Vector.Unboxed.Mutable.Dynamic as Dyn
                                (unstableSort, accessUnderlying, length,
                                 clear, pushBack )
import           Imj.Graphics.Color.Types
import           Imj.Graphics.Render.Delta.Types
import           Imj.Graphics.Render.Delta.Buffers
import           Imj.Graphics.Render.Delta.Cell
import           Imj.Graphics.Render.Delta.Clear
import           Imj.Graphics.Render.Delta.Draw
import           Imj.Graphics.Render.Delta.Internal.Types


-- | Flushes the frame, i.e renders it to the console.
--   Then, resizes the context if needed (see 'ResizePolicy')
--   and clears the back buffer (see 'ClearPolicy').
deltaFlush :: IORef Buffers -> IO ()
deltaFlush ref =
  readIORef ref
  >>= \buffers@(Buffers _ _ _ _ _ policies) -> do
        maySize <- shouldAdjustSize buffers
        maybe
          (render DeltaMode buffers)
          -- We force to render everything when size changes because
          -- the terminal will have moved content on resize:
          (\sz -> do
            b <- uncurry (createBuffers policies) sz
            writeIORef ref b
            initializeWithContent buffers b
            render FullMode b)
            maySize
  >> hFlush stdout -- TODO is flush blocking? slow? could it be async?

data RenderMode = DeltaMode | FullMode

-- | Note that the 'Scissor' is not taken into account here.
-- We could take it into account, if needed.
render :: RenderMode -> Buffers -> IO ()
render mode buffers@(Buffers (Buffer b) _ width _ (Delta delta) _) = do
  case mode of
    DeltaMode -> computeDelta buffers 0
    FullMode ->
      mapM_
        (\i -> do
          v <- read b i
          Dyn.pushBack delta $ mkIndexedCell v $ fromIntegral i)
        [0..pred $ length b]

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
            -> Dim BufferSize
            -> Dim Width
            -> Dim BufferIndex
            -> Maybe LayeredColor
            -> Maybe (Dim BufferIndex)
            -> IO LayeredColor
renderDelta delta size width index prevColors prevIndex
 | fromIntegral size == index =
    return whiteOnBlack -- the value is not used
 | otherwise = do
    c <- read delta $ fromIntegral index
    let (bg, fg, idx, char) = expandIndexed c
        prevRendered = (== Just (pred idx)) prevIndex
    setCursorPositionIfNeeded width idx prevRendered
    usedColor <- renderCell bg fg char prevColors
    renderDelta delta size width (succ index) (Just usedColor) (Just idx)


computeDelta :: Buffers
             -> Dim BufferIndex
             -- ^ the buffer index
             -> IO ()
computeDelta
 b@(Buffers (Buffer backBuf) (Buffer frontBuf) _ _ (Delta delta) _)
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

-- TODO merge with color change command to save 2 bytes
-- | The command to set the cursor position to 123,45 is "\ESC[123;45H",
-- its size is 9 bytes : one order of magnitude more than the size
-- of a char, so we avoid sending this command when not strictly needed.
{-# INLINE setCursorPositionIfNeeded #-}
setCursorPositionIfNeeded :: Dim Width
                          -> Dim BufferIndex
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
  when shouldSetCursorPosition $ Prelude.putStr $ setCursorPositionCode (fromIntegral rowIdx) (fromIntegral colIdx)

setCursorPositionCode :: Int -- ^ 0-based row to move to
                      -> Int -- ^ 0-based column to move to
                      -> String
setCursorPositionCode n m = csi [n + 1, m + 1] "H"

{-# INLINE renderCell #-}
renderCell :: Color8 Background
           -> Color8 Foreground
           -> Char
           -> Maybe LayeredColor
           -> IO LayeredColor
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
      sgrs = concat $ [color8FgSGRToCode fg | fgChange] ++
                      [color8BgSGRToCode bg | bgChange]

  if bgChange || fgChange
    then
      Prelude.putStr $ csi sgrs "m" ++ [char]
    else
      Prelude.putChar char
  return $ LayeredColor bg usedFg


csi :: [Int]
    -> String
    -> String
csi args code = "\ESC[" ++ intercalate ";" (map show args) ++ code


{-# INLINE xyFromIndex #-}
xyFromIndex :: Dim Width -> Dim BufferIndex -> (Dim Col, Dim Row)
xyFromIndex width idx =
  getRowCol idx width

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
