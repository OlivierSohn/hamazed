{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_HADDOCK hide #-}

module Imj.Graphics.Render.Delta.Draw
            ( fill
            , deltaDrawChar
            , deltaDrawChars
            , deltaDrawStr
            , deltaDrawTxt
            , module Imj.Graphics.Color
            , module Imj.Geo.Discrete.Types
            , String
              -- utilities
            , fillBackBuffer
            ) where

import           Imj.Prelude

import           Data.IORef( IORef , readIORef )
import           Data.Text(Text, unpack)
import           Data.Vector.Unboxed.Mutable( write, set, length )

import           Imj.Geo.Discrete
import           Imj.Geo.Discrete.Types
import           Imj.Graphics.Color
import           Imj.Graphics.Render.Delta.Internal.Types
import           Imj.Graphics.Render.Delta.Types
import           Imj.Graphics.Render.Delta.Cell


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
  readIORef ref
    >>= \(Buffers back@(Buffer b) _ width _ _) -> do
      let size = fromIntegral $ length b
      writeToBack back (indexFromPos size width pos) (mkCell colors c)


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
  readIORef ref
    >>= \(Buffers back@(Buffer b) _ width _ _) -> do
      let cell = mkCell colors c
          size = fromIntegral $ length b
      mapM_
        (\i -> writeToBack back (indexFromPos size width (move i RIGHT pos)) cell)
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
  readIORef ref
    >>= \(Buffers back@(Buffer b) _ width _ _) -> do
      let size = fromIntegral $ length b
      mapM_
        (\(c, i) ->
            writeToBack back (indexFromPos size width (move i RIGHT pos)) (mkCell colors c))
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


{-# INLINE writeToBack #-}
writeToBack :: Buffer Back -> Maybe (Dim BufferIndex) -> Cell -> IO ()
writeToBack _ Nothing _ = return ()
writeToBack (Buffer b) (Just pos) cell =
  write b (fromIntegral pos) cell


-- | Fills the entire area with a colored char.
fill :: Char
     -> LayeredColor
     -> IORef Buffers
     -> IO ()
fill char colors ioRefBuffers =
  readIORef ioRefBuffers
    >>= flip fillBackBuffer (mkCell colors char)


fillBackBuffer :: Buffers
               -> Cell
               -> IO ()
fillBackBuffer (Buffers (Buffer b) _ _ _ _) =
  set b


{-# INLINE indexFromPos #-}
indexFromPos :: Dim Size -> Dim Width -> Coords Pos -> Maybe (Dim BufferIndex)
indexFromPos size width (Coords y x) =
  if x >= fromIntegral width
    then Nothing
    else
      let idx = fromIntegral y * fromIntegral width + fromIntegral x
      in if idx < size
        then Just $ fromIntegral idx
        else Nothing
