{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Graphics.Class.HasSizedFace
    ( HasSizedFace(..)
    , SizedFace(..)
    , withSizedFace
    , withFontFace
    , setPixelSizes
    ) where

import           Imj.Prelude

import           Control.Exception(bracket)

import           FreeType

import           Imj.Geo.Discrete.Types


class HasSizedFace e where
  getSizedFace :: e -> SizedFace


data SizedFace = SizedFace {
    _sizedFace :: {-# UNPACK #-}  !FT_Face
  , _sz :: {-# UNPACK #-} !Size
}

-- 'setPixelSizes' can be expensive so we want to avoid calling it more than is necessary.
withSizedFace :: FilePath -> Size -> (SizedFace -> IO a) -> FT_Library -> IO a
withSizedFace path sz f =
  withFontFace path (\font -> do
    setPixelSizes font sz
    f $ SizedFace font sz)

withFontFace :: FilePath -> (FT_Face -> IO a) -> FT_Library -> IO a
withFontFace fn m lib = bracket bra ket m
  where
    bra = ft_New_Face lib fn 0
    ket = ft_Done_Face

setPixelSizes :: FT_Face -> Size -> IO ()
setPixelSizes face (Size h w) =
  ft_Set_Pixel_Sizes face (fromIntegral w) (fromIntegral h)
