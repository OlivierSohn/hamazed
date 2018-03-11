{-# LANGUAGE NoImplicitPrelude #-}

module Test.Imj.FreeType2
         ( testFreeType2
         ) where

import           Imj.Prelude
import           Prelude(FilePath, putStrLn, print, length)

import           Control.Exception

import           Foreign.Storable
import           Foreign.Marshal
import           Foreign.Ptr
import           Foreign.C

import           Graphics.Rendering.FreeType.Internal
import           Graphics.Rendering.FreeType.Internal.Bitmap(width, rows, buffer)
import           Graphics.Rendering.FreeType.Internal.Face(glyph, FT_Face)
import           Graphics.Rendering.FreeType.Internal.Vector(FT_Vector(FT_Vector))
import           Graphics.Rendering.FreeType.Internal.Library(FT_Library)
import           Graphics.Rendering.FreeType.Internal.GlyphSlot(advance
                                        , bitmap_left, bitmap_top
                                        , bitmap)
import           Graphics.Rendering.FreeType.Internal.PrimitiveTypes(FT_ULong, ft_LOAD_RENDER)

import           Data.Typeable

import           Imj.Graphics.Font

gray256ToChar :: Int -> Char
gray256ToChar i = levels2 !! idx
 where
  idx = quot (i * length levels2) 256
  -- http://paulbourke.net/dataformats/asciiart/
  --levels = reverse "$@B%8&WM#*oahkbdpqwmZO0QLCJUYXzcvunxrjft/\\|()1{}[]?-_+~<>i!lI;:,\"^`'. "
  levels2 = " .:-=+*#%@"

testFreeType2 :: IO ()
testFreeType2 =
  withTempFontFile content name go
 where
  (content,name) = getFont 0 fonts
  fonts = mkFontsVariations
  go path =
   withFreeType $ withFontFace path $ \face -> do
    ft "Set_Pixel_Sizes" $ ft_Set_Pixel_Sizes face 10 10
    slot <- peek $ glyph face
    forCharM_ face $ \c -> do
      print c
      ft "Load_Char" $ ft_Load_Char face c ft_LOAD_RENDER

      FT_Vector x _ <- peek $ advance slot
      case quotRem x 64 of
        (s, 0) -> putStrLn $ show c ++ '\t': show s
        _      -> fail "rem font size"

      bm <- peek $ bitmap slot
      left <- peek $ bitmap_left slot
      top <- peek $ bitmap_top slot

      let w = fromIntegral $ width bm
          h = fromIntegral $ rows bm
          buf = buffer bm
      print (w,h, left, top)
      putStrLn ""
      mapM_
        (\j ->
          mapM
            (\i -> do
              let idx = i + j * w
              grayValSigned <- fromIntegral <$> peekElemOff buf idx
              let grayVal = if grayValSigned < 0 then 256 + grayValSigned else grayValSigned :: Int
              return $ gray256ToChar grayVal
              )
            [0..pred w] >>= putStrLn. (++) "      ")
        [0..pred h]
      putStrLn ""
      {-
      mapM_
        (\j ->
          intercalate " " <$> mapM
            (\i -> do
              let idx = i + j * w
              grayValSigned <- fromIntegral <$> peekElemOff buf idx
              let grayVal = if grayValSigned < 0 then 256 + grayValSigned else grayValSigned :: Int
              return $ take 4 $ show grayVal ++ repeat ' '
              )
            [0..pred w] >>= putStrLn)
        [0..pred h]
      -}

      return ()


data FTError = FTError String CInt deriving(Show, Typeable)
instance Exception FTError

ft :: String -> IO CInt -> IO ()
ft n f = do
    e <- f
    unless (e == 0) $ throwIO (FTError n e)

withFreeType :: (FT_Library -> IO a) -> IO a
withFreeType = bracket bra ket
  where
    bra = alloca $ \p -> ft "Init_FreeType" (ft_Init_FreeType p) >> peek p
    ket = ft "Done_FreeType" . ft_Done_FreeType

withFontFace :: FilePath -> (FT_Face -> IO a) -> FT_Library -> IO a
withFontFace fn m lib = bracket bra ket m
  where
    bra = alloca $ \p -> withCString fn $ \f ->
        ft "New_Face" (ft_New_Face lib f 0 p) >> peek p
    ket = ft "Done_Face" . ft_Done_Face

firstChar :: FT_Face -> IO FT_ULong
firstChar face = fromIntegral <$> ft_Get_First_Char face nullPtr

nextChar :: FT_Face -> FT_ULong -> IO FT_ULong
nextChar face c = ft_Get_Next_Char face c nullPtr

forCharM_ :: FT_Face -> (FT_ULong -> IO ()) -> IO ()
forCharM_ face m = firstChar face >>= go
  where
    go 0 = return ()
    go i = m i >> nextChar face i >>= go
