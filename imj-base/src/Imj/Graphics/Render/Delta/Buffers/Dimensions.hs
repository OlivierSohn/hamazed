{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_HADDOCK hide #-}

module Imj.Graphics.Render.Delta.Buffers.Dimensions
        ( getDimensions
        , bufferSizeFromWH
        ) where

import           Imj.Prelude

import           Data.Word( Word16, Word32 )

import           System.Console.Terminal.Size as Term (size, Window(..))

import           Imj.Graphics.Render.Delta.Types


getDimensions :: ResizePolicy -> IO (Dim Width, Dim Height)
getDimensions (FixedSize w h) =
  return (w,h)
getDimensions MatchTerminalSize =
  maybe
    (Dim 300, Dim 90) -- sensible default values in case we fail to get terminal size
    (\(Term.Window h w) -> (Dim w, Dim h))
    <$> Term.size



bufferSizeFromWH :: Dim Width -> Dim Height -> (Dim BufferSize, Dim Width)
bufferSizeFromWH (Dim w') (Dim h') =
  let w = max 1 w'
      h = max 1 h'
      sz = fromIntegral w * fromIntegral h :: Word32
  -- indexed cells use a Word16 index so we can't exceed the Word16 maxBound
  in if sz > fromIntegral (maxBound :: Word16)
       then
         error $ "buffer size cannot be bigger than " ++ show (maxBound :: Word16) ++
            " : " ++ show (sz, w, h)
       else
         (Dim $ fromIntegral sz, Dim w)
