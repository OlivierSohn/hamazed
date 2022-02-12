{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}

module Test.Imj.FreeType2
         ( testFreeType2
         , testFreeType2'
         ) where

import           Imj.Prelude
import           Prelude(putStrLn, print, length)

import           Control.Monad.Reader(runReaderT)

import           Foreign.Storable
import           Foreign.Ptr

import           Graphics.Rendering.FreeType.Internal
import           Graphics.Rendering.FreeType.Internal.Bitmap(width, rows, buffer)
import           Graphics.Rendering.FreeType.Internal.Face(glyph, FT_Face)
import           Graphics.Rendering.FreeType.Internal.GlyphSlot(
                                          bitmap_left, bitmap_top
                                        , bitmap)
import           Graphics.Rendering.FreeType.Internal.PrimitiveTypes(FT_ULong, FT_UInt, ft_LOAD_RENDER)

import           Data.Char(chr)

import           System.Console.ANSI(clearScreen)

import           Imj.Geo.Discrete.Types

import           Imj.Graphics.Color
import           Imj.Graphics.Class.HasSizedFace
import qualified Imj.Graphics.Class.Positionable as Pos
import           Imj.Graphics.Font
import           Imj.Graphics.Render
import           Imj.Graphics.Render.Naive
import           Imj.Graphics.Text.RasterizedString

testFreeType2 :: IO ()
testFreeType2 =
  withTempFontFile content name go
 where
  -- the index passed should correspond to a bitmap font ? 0 (VCR font) didn't work
  (content,name) = getFont 1 fonts
  h = 10
  sz = Size h 10
  fonts = mkFontsVariations
  go path =
    withFreeType $ withFontFace path $ \face -> do
      setPixelSizes face sz
      forCharM_ face $ drawFaceChar face
      forM_
        "Hello"
        (getCharIndex face >=> maybe (putStrLn "unknown char") (drawFaceIndex face))


testFreeType2' :: IO ()
testFreeType2' =
  forM_ [0..fromIntegral $ pred $ nFonts fonts] $ \i -> do
    -- the index passed should correspond to a bitmap font ? 0 (VCR font) didn't work
    let (content,name) = getFont i fonts
    print i
    withTempFontFile content name go
 where
  h = 20
  sz = Size h 20
  ref = Coords 5 5
  center = Coords 30 100
  str = "Hello Jim"
  fonts = mkFontsVariations
  go path =
    withFreeType $ withSizedFace path sz $ \face -> do
      clearScreen
      _ <- flip runReaderT NaiveDraw $ do
        rstr <- liftIO $ mkRasterizedString str grayGradient face
        Pos.drawAt rstr ref
        drawVerticallyCentered center rstr
        drawGlyph (textGlyph 'C') center whiteOnBlack
        renderToScreen
      return ()

gray256ToChar :: Int -> Char
gray256ToChar 0 = ' '
gray256ToChar i = levels !! idx
 where
  idx = quot ((i-1) * length levels) 255
  -- http://paulbourke.net/dataformats/asciiart/
  --levels = reverse "$@B%8&WM#*oahkbdpqwmZO0QLCJUYXzcvunxrjft/\\|()1{}[]?-_+~<>i!lI;:,\"^`'."
  levels = ".:-=+*#%@"

drawFaceChar :: FT_Face -> Char -> IO ()
drawFaceChar face c = do
  loadChar face c
  drawItem face

drawFaceIndex :: FT_Face -> FT_UInt -> IO ()
drawFaceIndex face i = do
  ft "Load_Glyph" $ ft_Load_Glyph face i ft_LOAD_RENDER
  drawItem face

drawItem :: FT_Face -> IO ()
drawItem face = do
  slot <- peek $ glyph face
  {-
  FT_Vector x _ <- peek $ advance slot
  case quotRem x 64 of
    (s, 0) -> putStrLn $ show c ++ '\t': show s
    _      -> error "rem font size"
  -}
  bm <- peek $ bitmap slot
  left <- peek $ bitmap_left slot
  top <- peek $ bitmap_top slot

  let w = fromIntegral $ width bm
      h = fromIntegral $ rows bm
      buf = buffer bm
  print (w,h, left, top) -- bounding box of non zero intensities
  putStrLn ""
  forM_
    [0..pred h]
    (\j ->
      forM
        [0..pred w]
        (\i -> do
          let idx = i + j * w
          grayValSigned <- fromIntegral <$> peekElemOff buf idx
          let grayVal = if grayValSigned < 0 then 256 + grayValSigned else grayValSigned :: Int
          return $ gray256ToChar grayVal)
        >>= putStrLn . (++) "      ")

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


firstChar :: FT_Face -> IO FT_ULong
firstChar face = fromIntegral <$> ft_Get_First_Char face nullPtr

nextChar :: FT_Face -> FT_ULong -> IO FT_ULong
nextChar face c = ft_Get_Next_Char face c nullPtr

forCharM_ :: FT_Face -> (Char -> IO ()) -> IO ()
forCharM_ face m =
  firstChar face >>= go
 where
  go 0 = return ()
  go i = do
    m $ chr $ fromIntegral i
    nextChar face i >>= go
