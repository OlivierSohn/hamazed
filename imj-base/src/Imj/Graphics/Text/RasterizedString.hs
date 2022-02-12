{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}

module Imj.Graphics.Text.RasterizedString
         ( RasterizedString
         , mkRasterizedString
         , mkRasterizedStringFromColorString
         , drawVerticallyCentered
         -- * Color functions
         , grayGradient
         -- * TODO split
         , withFreeType
         , loadChar
         , getCharIndex
         ) where

import           Imj.Prelude

import           Control.Exception(bracket)

import           Foreign.Storable(peek, peekElemOff)

import           FreeType

import           Data.Char(ord)

import           Imj.Geo.Discrete.Types

import           Imj.Graphics.Class.HasSizedFace
import qualified Imj.Graphics.Class.Positionable as Pos
import           Imj.Graphics.Color
import           Imj.Graphics.Font
import           Imj.Graphics.Render
import           Imj.Graphics.Text.ColorString
import           Imj.Util

type CharPosition = Int
type ColorFunc = CharPosition -> Int -> LayeredColor
data RasterizedString = RasterizedString {
    _str :: !String
  , _face :: !SizedFace
  , _colorFunc :: ColorFunc
  , _srBBox :: !Size
}
instance Pos.Positionable RasterizedString where
  height (RasterizedString _ _ _ (Size h _)) = h
  width (RasterizedString _ _ _ (Size _ w)) = w
  drawAt (RasterizedString str (SizedFace face _) color (Size h _)) (Coords refY refX) =
    rasterizeString face str 1 $ \i char c@(Coords y x) value ->
      when (value > 0) $
        drawGlyph (textGlyph char) (sumCoords ref $ Coords (-y) x) $ color i $ dist c
   where
    ref = Coords (refY + fromIntegral h) refX

{-# INLINE dist #-}
dist :: Coords Pos -> Int
dist (Coords (Coord y) (Coord x)) = x+y

mkRasterizedString :: String -> ColorFunc -> SizedFace -> IO RasterizedString
mkRasterizedString str color sizedFace@(SizedFace face _) =
  RasterizedString str sizedFace color <$> getRasterizedStringSize face str 1

mkRasterizedStringFromColorString :: ColorString -> SizedFace -> IO RasterizedString
mkRasterizedStringFromColorString cs face = do
  let s = destructure cs
      str = map fst s
      colors = map snd s
  mkRasterizedString str (funcFromColors colors) face

grayGradient :: Int -> Int -> LayeredColor
grayGradient _ d =
  let v = quot (d+2) 6
  --in interpolate (rgb 2 1 0) (rgb 5 4 0) v
  in onBlack $ gray $ clamp (4 + 2*v) 0 23

funcFromColors :: [LayeredColor] -> Int -> Int -> LayeredColor
funcFromColors [] _ _ = error "logic" -- colorstring is supposed to have an equal number of char and colors
funcFromColors (c:_) 0 _ = c
funcFromColors (_:colors) charIdx i = funcFromColors colors (pred charIdx) i

drawVerticallyCentered :: (Pos.Positionable a
                         , MonadIO m
                         , MonadReader e m, Draw e)
                       => Coords Pos
                       -- ^ ref Coords
                       -> a
                       -> m ()
drawVerticallyCentered (Coords yRef xRef) p = do
  _ <- Pos.drawAligned p $ Pos.mkCentered $ Coords (yRef - quot (fromIntegral h) 2) xRef
  return ()
 where
  h = Pos.height p

charForSizeOfSpace :: Char
charForSizeOfSpace = '-'

getRasterizedStringSize :: FT_Face
                        -> String
                        -- ^ The title text
                        -> Length Width
                        -- ^ The number of whitespace between letters
                        -> IO Size
getRasterizedStringSize face str interLetterSpaces =
  foldM
    (\(wi,he) c -> do
      if c == ' '
        then
          loadChar face charForSizeOfSpace
        else
          loadChar face c
      slot <- frGlyph <$> peek face
      bm <- gsrBitmap <$> peek slot
      return (wi + fromIntegral (bWidth bm) + interLetterSpaces
            , max he $ fromIntegral $ bRows bm))
    (-interLetterSpaces, 0)
    str
    >>= \(accW, accH) -> return $ Size accH (max 0 accW)

rasterizeString :: (MonadIO m)
                => FT_Face
                -> String
                -- ^ The title text
                -> Length Width
                -- ^ The number of whitespace between letters
                -> (Int -> Char -> Coords Pos -> Int -> m ())
                -> m ()
rasterizeString face str interLetterSpaces f =
  foldM_
    (\(idx,pos) c -> do
      liftIO $ if c == ' '
              then
                loadChar face charForSizeOfSpace
              else
                loadChar face c
      slot <- liftIO $ (frGlyph <$> peek face)
      bm <- liftIO $ (gsrBitmap <$> peek slot)
      let w = fromIntegral $ bWidth bm
          h = fromIntegral $ bRows bm
          buf = bBuffer bm
      when (c /= ' ') $
       forM_
        [0..pred h :: Int]
        (\j ->
          forM_
            [0..pred w :: Int]
            (\i -> do
              signed <- fromIntegral <$> liftIO (peekElemOff buf $ i + j * w)
              let unsigned =
                    if signed < 0
                      then
                        256 + signed
                      else
                        signed :: Int
              f idx c (sumCoords pos $ Coords (fromIntegral (pred h) - fromIntegral j) (fromIntegral i)) unsigned))
      return (succ idx, Pos.move (w + fromIntegral interLetterSpaces) RIGHT pos))
    (0,zeroCoords) str

------------------------ -- TODO split

loadChar :: FT_Face -> Char -> IO ()
loadChar face c = ft_Load_Char face (fromIntegral $ ord c) FT_LOAD_RENDER

------------------------ -- TODO split

withFreeType :: (FT_Library -> IO a) -> IO a
withFreeType = bracket ft_Init_FreeType ft_Done_FreeType

getCharIndex :: FT_Face -> Char -> IO (Maybe FT_UInt)
getCharIndex f c =
  (\case
    0 -> Nothing
    i -> Just i) <$> ft_Get_Char_Index f (fromIntegral $ ord c)
