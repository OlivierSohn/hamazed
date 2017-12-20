{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_HADDOCK hide #-}

module Render.Delta.Draw
            ( fill
            , deltaDrawChar
            , deltaDrawChars
            , deltaDrawStr
            , deltaDrawTxt
            , module Color
            , module Geo.Discrete.Types
            , String
              -- utilities
            , fillBackBuffer
            ) where

import           Imajuscule.Prelude

import           Data.IORef( IORef , readIORef )
import           Data.String(String)
import           Data.Text(Text, unpack)
import           Data.Vector.Unboxed.Mutable( write, set, length )

import           Color
import           Geo.Discrete.Types
import           Render.Delta.Internal.Types
import           Render.Delta.Cell
import           Render.Delta.Types


{-# INLINABLE deltaDrawChar #-}
-- | Draw a 'Char'
deltaDrawChar :: IORef Buffers
              -> Char
              -> Coords
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
               -> Coords
               -- ^ Location of left-most 'Char'
               -> LayeredColor
               -- ^ Background and foreground colors
               -> IO ()
deltaDrawChars ref count c pos colors =
  readIORef ref
    >>= \(Buffers back@(Buffer b) _ width _ _) -> do
      let cell = mkCell colors c
          size = fromIntegral $ length b
          idx = indexFromPos size width pos
      mapM_
        (\i -> let idx' = (fromIntegral idx + i) `fastMod` size
               in writeToBack back idx' cell)
        [0..pred count]


{-# INLINABLE deltaDrawStr #-}
-- | Draw a 'String'
deltaDrawStr :: IORef Buffers
             -> String
             -> Coords
             -- ^ Location of first 'Char'
             -> LayeredColor
             -- ^ Background and foreground colors
             -> IO ()
deltaDrawStr ref str pos colors =
  readIORef ref
    >>= \(Buffers back@(Buffer b) _ width _ _) -> do
      let size = fromIntegral $ length b
          idx = indexFromPos size width pos
      mapM_
        (\(c, i) ->
            writeToBack back (idx+i `fastMod` size) (mkCell colors c))
        $ zip str [0..]

{-# INLINABLE deltaDrawTxt #-}
-- | Draw a 'Text'
deltaDrawTxt :: IORef Buffers
             -> Text
             -> Coords
             -- ^ Location of first 'Char'
             -> LayeredColor
             -- ^ Background and foreground colors
             -> IO ()
deltaDrawTxt ref text = deltaDrawStr ref $ unpack text


{-# INLINE writeToBack #-}
writeToBack :: Buffer Back -> Dim BufferIndex -> Cell -> IO ()
writeToBack (Buffer b) pos = write b (fromIntegral pos)


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
indexFromPos :: Dim Size -> Dim Width -> Coords -> Dim BufferIndex
indexFromPos size width (Coords y x) =
  (fromIntegral y * fromIntegral width + fromIntegral x) `fastMod` size


-- | Modulo optimized for cases where most of the time,
--    a < b (for a mod b)
{-# INLINE fastMod #-}
fastMod :: Int -> Dim Size -> Dim BufferIndex
fastMod a b'
  | 0 <= a && a < b = fromIntegral a          -- fast path
  | otherwise       = fromIntegral $ a `mod` b  -- slow path
  where b = fromIntegral b'
