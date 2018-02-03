{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_HADDOCK hide #-}

module Imj.Graphics.Render.Delta.Draw
            ( fill
            , deltaFill
            , deltaDrawChar
            , deltaDrawChars
            , deltaDrawStr
            , deltaDrawTxt
            , module Imj.Graphics.Color
            , module Imj.Geo.Discrete.Types
            , String
              -- utilities
            , initializeWithContent
            , fillBackBuffer
            , deltaForgetFrontValues
            ) where

import           Imj.Prelude

import           Data.IORef( IORef , readIORef )
import           Data.Text(Text, unpack)
import           Data.Vector.Unboxed.Mutable( unsafeWrite, set, length, unsafeSlice, unsafeCopy )

import           Imj.Geo.Discrete
import           Imj.Geo.Discrete.Types
import           Imj.Graphics.Color
import           Imj.Graphics.Render.Delta.Internal.Types
import           Imj.Graphics.Render.Delta.Types
import           Imj.Graphics.Render.Delta.Buffers
import           Imj.Graphics.Render.Delta.Cell
import           Imj.Graphics.Render.Delta.Cells
import           Imj.Graphics.UI.RectArea


{-# INLINABLE deltaDrawChar #-}
-- | Draw a 'Char'
deltaDrawChar :: IORef Buffers
              -> Char
              -> Coords Pos
              -- ^ Location
              -> LayeredColor
              -- ^ Background and foreground colors
              -> IO ()
deltaDrawChar ref c pos colors =
  readIORef ref >>= \(Buffers back@(Buffer b) _ width scissor _ _) -> do
    let size = fromIntegral $ length b
    writeToBack back (indexFromPos size width scissor pos) (mkCell colors c)


{-# INLINABLE deltaDrawChars #-}
-- | Draws a 'Char' multiple times, starting at the given coordinates and then moving to the right.
--
-- @deltaDrawChars n c@ should be faster than @deltaDrawStr (repeat n c)@,
-- as the encoding of information in a 'Cell' happens once only. (TODO verify in GHC core with optimizations)
deltaDrawChars :: IORef Buffers
               -> Int
               -- ^ Number of chars to draw
               -> Char
               -> Coords Pos
               -- ^ Location of left-most 'Char'
               -> LayeredColor
               -- ^ Background and foreground colors
               -> IO ()
deltaDrawChars ref count c pos colors =
  readIORef ref >>= \(Buffers back@(Buffer b) _ width scissor _ _) -> do
    let cell = mkCell colors c
        size = fromIntegral $ length b
    mapM_
      (\i -> writeToBack back (indexFromPos size width scissor (move i RIGHT pos)) cell)
      [0..pred count]

{-# INLINABLE deltaDrawStr #-}
-- | Draw a 'String'
deltaDrawStr :: IORef Buffers
             -> String
             -> Coords Pos
             -- ^ Location of first 'Char'
             -> LayeredColor
             -- ^ Background and foreground colors
             -> IO ()
deltaDrawStr ref str pos colors =
  readIORef ref >>= \(Buffers back@(Buffer b) _ width scissor _ _) -> do
    let size = fromIntegral $ length b
    mapM_
      (\(c, i) ->
          writeToBack back (indexFromPos size width scissor (move i RIGHT pos)) (mkCell colors c))
      $ zip str [0..]

{-# INLINABLE deltaDrawTxt #-}
-- | Draw a 'Text'
deltaDrawTxt :: IORef Buffers
             -> Text
             -> Coords Pos
             -- ^ Location of first 'Char'
             -> LayeredColor
             -- ^ Background and foreground colors
             -> IO ()
deltaDrawTxt ref text = deltaDrawStr ref $ unpack text

-- | Fills the scissor area with a colored char.
deltaFill :: IORef Buffers -> Char -> LayeredColor -> IO ()
deltaFill ref c color =
  readIORef ref >>= \(Buffers back@(Buffer b) _ width scissor _ _) -> do
    let height = getHeight width $ fromIntegral $ length b
        drawableArea =
          mkRectArea zeroCoords $ Size (fromIntegral height) $ fromIntegral width
        region@(RectArea (Coords r1 c1) (Coords r2 c2)) =
          intersection scissor drawableArea
        nCells = 1 + fromIntegral (c2 - c1)
        cell = mkCell color c
    unless (isEmpty region) $
      mapM_
        (\r -> do
          let leftMostCoords = Coords r $ assert (c1 <= c2) c1
              idx = unsafeIndexFromPos width leftMostCoords
          writeNToBack back idx nCells cell)
        [r1..r2]

{-# INLINE writeToBack #-}
writeToBack :: Buffer Back -> Maybe (Dim BufferIndex) -> Cell -> IO ()
writeToBack _ Nothing _ = return ()
writeToBack (Buffer b) (Just pos) cell =
  unsafeWrite b (fromIntegral pos) cell

writeNToBack :: Buffer Back -> Dim BufferIndex -> Int -> Cell -> IO ()
writeNToBack (Buffer b) pos n cell = do
  let startIdx = fromIntegral pos
  mapM_ (\i -> unsafeWrite b i cell) [startIdx..startIdx+n-1]

-- | Fills the entire area with a colored char, doesn't take 'Scissor' into account.
fill :: Char
     -> LayeredColor
     -> IORef Buffers
     -> IO ()
fill char colors ref =
  readIORef ref >>= flip fillBackBuffer (mkCell colors char)


fillBackBuffer :: Buffers
               -> Cell
               -> IO ()
fillBackBuffer (Buffers (Buffer b) _ _ _ _ _) =
  set b

deltaForgetFrontValues :: IORef Buffers -> IO ()
deltaForgetFrontValues ref =
  readIORef ref >>= \(Buffers _ (Buffer f) _ _ _ _) ->
    set f invalidCell

-- | Use if you know that the 'Coords' is inside the 'Scissor' and inside the
-- region of the screen represented by the back buffer.
--
-- If in doubt, use 'indexFromPos'.
unsafeIndexFromPos :: Dim Width -> Coords Pos -> Dim BufferIndex
unsafeIndexFromPos width (Coords y x) =
  fromIntegral y * fromIntegral width + fromIntegral x
{-# INLINE unsafeIndexFromPos #-}

-- | Returns 'Nothing' if the 'Coords' is outside the 'Scissor', or outside the region of
-- the screen represented by the back buffer.
indexFromPos :: Dim BufferSize -> Dim Width -> Scissor -> Coords Pos -> Maybe (Dim BufferIndex)
indexFromPos size width scissor coords@(Coords y x)
  -- is the position inside the scissor?
  | not $ scissor `contains` coords  = Nothing
  -- is the column position within the handled column indexes ?
  | x < 0 || y < 0 || x >= fromIntegral width || y >= fromIntegral height = Nothing
  | otherwise =
      let idx = unsafeIndexFromPos width coords
      in if idx < fromIntegral size
        then Just $ fromIntegral idx
        else Nothing
 where
  height = getHeight width size

-- | Copy the backbuffer content of the first 'Buffers' back buffer to the other
-- 'Buffers' back /and/ front buffers
initializeWithContent :: Buffers -> Buffers -> IO ()
initializeWithContent
  src@(Buffers (Buffer bSrc) _ _ _ _ _)
  tgt@(Buffers (Buffer bTgt) (Buffer fTgt) _ _ _ _) = do
  let (fromW, fromH) = getBufferDimensions src
      (toW, toH) = getBufferDimensions tgt
  mapM_
    (\rowIdx -> do
      let baseIdxFrom = fromIntegral $ rowIdx * fromIntegral fromW
          baseIdxTo   = fromIntegral $ rowIdx * fromIntegral toW
          countElems = fromIntegral $ min fromW toW
          sliceFrom = unsafeSlice baseIdxFrom countElems bSrc
          sliceTo1   = unsafeSlice baseIdxTo countElems bTgt
          sliceTo2   = unsafeSlice baseIdxTo countElems fTgt
      unsafeCopy sliceTo1 sliceFrom
      -- we also copy to the front buffer. for rendering, we will force to render everything
      -- so it's not a problem that back and front match.
      unsafeCopy sliceTo2 sliceFrom)
    [0..pred $ min fromH toH]
