
module Render.Backends.Internal.Draw
            ( drawChar
            , drawChars
            , drawStr
            , drawTxt
            , fill
            -- utilities
            , fillBackBuffer
            -- reexport
            , Text
            ) where


import           Data.IORef( IORef , readIORef )
import           Data.Text(Text, unpack)
import           Data.Vector.Unboxed.Mutable( write )

import           Color
import           Geo.Discrete.Types
import           Render.Backends.Internal.Types
import           Render.Backends.Internal.Cell
import           Render.Types

drawChar :: Char
         -> Coords
         -> LayeredColor
         -> IORef Buffers
         -> IO (IORef Buffers)
drawChar c pos colors ioRefBuffers =
  readIORef ioRefBuffers
    >>= \b@(Buffers back _ _ _ _ _) -> do
      let idx = indexFromPos b pos
      writeToBack back idx (mkCell colors c)
      return ioRefBuffers


-- | Draws multiple times the same 'Char'
drawChars :: Int
          -- ^ Number of repetitions.
          -> Char
          -> Coords
          -> LayeredColor
          -> IORef Buffers
          -> IO (IORef Buffers)
drawChars count c pos colors ioRefBuffers =
  readIORef ioRefBuffers
    >>= \b@(Buffers back _ size _ _ _) -> do
      let cell = mkCell colors c
          idx = indexFromPos b pos
      mapM_
        (\i -> let idx' = (fromIntegral idx + i) `fastMod` size
               in writeToBack back idx' cell)
        [0..pred count]
      return ioRefBuffers


drawStr :: String
        -> Coords
        -> LayeredColor
        -> IORef Buffers
        -> IO (IORef Buffers)
drawStr str pos colors ioRefBuffers =
  readIORef ioRefBuffers
    >>= \b@(Buffers back _ size _ _ _) -> do
      let idx = indexFromPos b pos
      mapM_
        (\(c, i) ->
            writeToBack back (idx+i `fastMod` size) (mkCell colors c))
        $ zip str [0..]
      return ioRefBuffers


drawTxt :: Text
        -> Coords
        -> LayeredColor
        -> IORef Buffers
        -> IO (IORef Buffers)
drawTxt text = drawStr $ unpack text


{-# INLINE writeToBack #-}
writeToBack :: Buffer Back -> Dim Index -> Cell -> IO ()
writeToBack (Buffer b) pos = write b (fromIntegral pos)


-- | Fills the entire canvas with a char.
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
fillBackBuffer (Buffers (Buffer b) _ size _ _ _) cell =
  mapM_ (\pos -> write b pos cell) [0..fromIntegral $ pred size]



{-# INLINE indexFromPos #-}
indexFromPos :: Buffers -> Coords -> Dim Index
indexFromPos (Buffers _ _ size width _ _) (Coords y x) =
  (fromIntegral y * fromIntegral width + fromIntegral x) `fastMod` size


-- | Modulo optimized for cases where most of the time,
--    a < b (for a mod b)
{-# INLINE fastMod #-}
fastMod :: Int -> Dim Size -> Dim Index
fastMod a b'
  | 0 <= a && a < b = fromIntegral a          -- fast path
  | otherwise       = fromIntegral $ a `mod` b  -- slow path
  where b = fromIntegral b'
