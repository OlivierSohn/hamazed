{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_HADDOCK hide #-}

module Imj.Graphics.Render.Delta.Buffers.Dimensions
        ( getPolicyDimensions
        , bufferSizeFromWH
        ) where

import           Imj.Prelude

import           Data.Word( Word16, Word32 )

import           Imj.Geo.Discrete.Types
import           Imj.Graphics.Render.Delta.Types


getPolicyDimensions :: ResizePolicy -> IO (Maybe Size) -> IO (Dim Width, Dim Height)
getPolicyDimensions (FixedSize w h) = \_ -> return (w,h)
getPolicyDimensions DynamicSize = fmap $ maybe
  (Dim 300, Dim 90)
  (\(Size h w) -> (fromIntegral w, fromIntegral h))

bufferSizeFromWH :: Dim Width -> Dim Height -> Either String (Dim BufferSize, Dim Width)
bufferSizeFromWH (Dim w') (Dim h') =
  let w = max 1 w'
      h = max 1 h'
      sz = fromIntegral w * fromIntegral h :: Word32
  -- indexed cells use a Word16 index so we can't exceed the Word16 maxBound
  in if sz > fromIntegral (maxBound :: Word16)
       then
         Left $ "buffer size cannot be bigger than " ++ show (maxBound :: Word16) ++
            " : " ++ show (sz, w, h)
       else
         Right (Dim $ fromIntegral sz, Dim w)
