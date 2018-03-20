{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Graphics.Class.HasSizedFace
    ( HasSizedFace(..)
    , SizedFace(..)
    , withSizedFace
    , withFontFace
    , ft
    , setPixelSizes
    ) where

import           Imj.Prelude
import           Prelude(FilePath)

import           Control.Exception(Exception(..), bracket, throwIO)
import           Data.Typeable(Typeable)

import           Foreign.Storable(peek)
import           Foreign.Marshal(alloca)
import           Foreign.C(CInt, withCString)

import           Graphics.Rendering.FreeType.Internal(ft_New_Face, ft_Done_Face, ft_Set_Pixel_Sizes)
import           Graphics.Rendering.FreeType.Internal.Face(FT_Face)
import           Graphics.Rendering.FreeType.Internal.Library(FT_Library)

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
    bra = alloca $ \p -> withCString fn $ \f ->
        ft "New_Face" (ft_New_Face lib f 0 p) >> peek p
    ket = ft "Done_Face" . ft_Done_Face

setPixelSizes :: FT_Face -> Size -> IO ()
setPixelSizes face (Size h w) =
  ft "Set_Pixel_Sizes" $ ft_Set_Pixel_Sizes face (fromIntegral w) (fromIntegral h)


data FTError = FTError String CInt deriving(Show, Typeable)
instance Exception FTError

ft :: String -> IO CInt -> IO ()
ft n f = do
    e <- f
    unless (e == 0) $ throwIO (FTError n e)
