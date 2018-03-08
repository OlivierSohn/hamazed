{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}

module Imj.Graphics.Font
    ( fontFiles
    , withTempFontFile
    , Fonts(..)
    , createFonts
    , lookupFont
    , Font(..)
    , Glyph(..)
    , decodeGlyph
    , textGlyph
    , gameGlyph
    , FontSpec(..)
    , PPU
    , mkUserPPU
    , half
    , floorToPPUMultiple
    , loadFont
    , showDetailed
    ) where

import           Imj.Prelude

import           Control.DeepSeq(NFData)
import           Data.Bits(shiftL, shiftR, (.&.), (.|.))
import           Data.ByteString(ByteString, writeFile)
import           Data.Char(chr, ord)
import           Data.Either(partitionEithers)
import           Data.List(foldl', length)
import           Data.FileEmbed(embedFile)
import           Data.Word(Word32, Word8)
import           Foreign.Ptr(nullPtr)
import           System.IO.Temp(withSystemTempDirectory)

import qualified Graphics.Rendering.FTGL as FTGL

import           Imj.Geo.Continuous
import           Imj.Geo.Discrete.Types
import           Imj.Util

{-| This is a font based on
<https://github.com/adobe-fonts/source-code-pro Source Code Pro Bold>,
modified to reduce the negative height
of the Pipe glyph used for hamazed game in animations. The reason I reduced
this height is so that the drawn pixels are kept within the logical square reserved
to that 'Coords', else, since I use delta rendering, there would be leftovers of
previous frames outside the boundaries. -}
fontFiles :: [ByteString]
-- Note that I don't embed the directory, I want compilation to fail if this particular file
-- is not found.
fontFiles = [$(embedFile "fonts/SrcCodPro-Bold-PipeReduced.ttf")
           , $(embedFile "fonts/04B_30__.TTF")]

-- I want O(1) access to any font. For now this is ok, as I use 2 fonts only.
-- in the future, maybe use an "Immutable Storable Vector".
data Fonts = Fonts {
    getFont0 :: {-# UNPACK #-} !Font
  , getFont1 :: {-# UNPACK #-} !Font
} deriving (Generic, Show, NFData)

data Font = Font {
    _ftglFont :: {-# UNPACK #-} !FTGL.Font
  , _offset :: {-# UNPACK #-} !(Vec2 Pos)
  -- ^ We use the /same/ offset for every character, to preserve font aspect.
} deriving(Generic, Show, NFData)

showDetailed :: Font -> IO String
showDetailed ft@(Font f _) = do
  details <- FTGL.getFontFaceSize f
  return $ show (ft, "size:", details)

lookupFont :: FontSpec -> Fonts -> Font
lookupFont (FontSpec 0) = getFont0
lookupFont (FontSpec 1) = getFont1
lookupFont (FontSpec n) = error $ "font index out of range : " ++ show n

newtype Glyph = Glyph Word32
  deriving(Generic, Show, Eq)
newtype FontSpec = FontSpec Word8
  deriving(Generic, Show)

gameGlyph :: Char -> Glyph
gameGlyph c = encodeGlyph c $ FontSpec 0

textGlyph :: Char -> Glyph
textGlyph c = encodeGlyph c $ FontSpec 1

-- unicode max code is OX10FFFF. Glyph being a 32-bit type, we have 11 high bits
-- to store font and metadata.
encodeGlyph :: Char -> FontSpec -> Glyph
encodeGlyph char (FontSpec s) =
  Glyph $ fromIntegral (ord char) .|. (fromIntegral s `shiftL` 24)

decodeGlyph :: Glyph -> (Char, FontSpec)
decodeGlyph (Glyph w) =
  (char, md)
 where
  char = chr $ fromIntegral $ w .&. 0xFFFFFF
  md = FontSpec $ fromIntegral $ w `shiftR` 24


-- | Pixels per units, in vertical and horizontal directions.
-- Both should be even, because we divide them by 2
-- to draw numbers in binary representation.
type PPU = Size

mkUserPPU :: Length Width -> Length Height -> Either String PPU
mkUserPPU w h
  | w < 4 || h < 4 = Left $ "PPU values should be >= 4. At least one of them is too small:" ++ show (w,h)
  | odd w || odd h = Left $ "PPU values should be even. At least one of them is odd:" ++ show (w,h)
  | otherwise = Right $ Size h w

{-# INLINE half #-}
half :: PPU -> PPU
half (Size h w) = assert (even h && even w) $ Size (quot h 2) (quot w 2)

floorToPPUMultiple :: Size -> PPU -> Size
floorToPPUMultiple (Size (Length h) (Length w)) (Size (Length ppuH) (Length ppuW)) =
  Size (fromIntegral $ f ppuH h)
       (fromIntegral $ f ppuW w)
 where
  f ppu l = ppu * quot l ppu

withTempFontFile :: Int -> (String -> IO a) -> IO a
withTempFontFile fontIdx act =
  withSystemTempDirectory "fontDir" $ \tmpDir -> do
    let filePath = tmpDir ++ "/font" ++ show fontIdx ++ ".ttf"
    -- we don't bother closing the file, it will be done by withSystemTempDirectory
    writeFile filePath (fontFiles!!fontIdx)
    act filePath

createFonts :: Int -> PPU -> IO (Either String Fonts)
createFonts i ppu = do
  gameFont <- f gameCharSet
  textFont <- f textCharSet
  let (errors, fonts) = partitionEithers [gameFont, textFont]
  if null errors
    then
      return $ Right $ mkFonts fonts
    else
      return $ Left $ show (length errors) ++
        " error(s) while creating the fonts :\n" ++ intercalate ",\n" errors
 where
  mkFonts [gameFont, textFont] = Fonts gameFont textFont
  mkFonts _ = error "logic"
  gameCharSet = CharSet $ ['0'..'9'] ++ ['a'..'f'] ++ "-=|+ZT_." -- TODO _ should be moved up for game.
  textCharSet = CharSet $ ['0'..'9'] ++ ['a'..'z'] ++ ['A'..'Z'] ++ ",.<>[](){}&|/\\$#%@?!^¨çàÀÇ"
  f charset = createFont charset i ppu

data ForOffset
newtype CharSet a = CharSet String
  deriving(Generic, Show)

createFont :: CharSet ForOffset -> Int -> PPU -> IO (Either String Font)
createFont charset i ppu@(Size ppuH _) =
  loadFont i >>= \font ->
  if font == nullPtr
    then
      return $ Left "nullPtr"
  else do
    let maxFontSize = 2 * fromIntegral ppuH
        minFontSize = 1
        !rectAABB = unitRectangleAABB ppu
        setSize x =
          FTGL.setFontFaceSize font x 72 >>= \err -> do
            when (err /= 1) $ -- according to http://ftgl.sourceforge.net/docs/html/FTFont_8h.html#00c8f893cbeb98b663f9755647a34e8c
              error $ "setFontFaceSize failed: " ++ show (x, err)
            -- verify that we can reliably use 'FTGL.getFontFaceSize' as indicator of the last set size:
            s <- FTGL.getFontFaceSize font
            when (s /= x) $
              error $ "Font sizes mismatch: " ++ show (s, x)
        condition x = do
          setSize x
          charsetAABB charset font >>= \aabb -> do
            let (_, offsetAABB) = offsetToCenterAABB aabb ppu
            -- return True if the (offset) bounding box of the character set is contained in the unit rectangle:
            return $ margin offsetAABB rectAABB > 0

    -- find the max font size such that the charset will be strictly contained
    -- in a unit rectangle.
    lastAbove False condition minFontSize maxFontSize >>= maybe
      (return $ Left $ "ppu is probably too small:" ++ show ppu)
      (\optimalSize -> do
          FTGL.getFontFaceSize font >>= \curSize ->
            -- maybe 'optimalSize' was not the last condition checked, in that case we set it again:
            when (curSize /= optimalSize) $ setSize optimalSize
          Right . Font font . fst . flip offsetToCenterAABB ppu <$> charsetAABB charset font)


loadFont :: Int -> IO FTGL.Font
loadFont i = withTempFontFile i $ \filePath ->
    --FTGL.createBitmapFont filePath -- gives brighter colors but the shape of letters is awkward.
    FTGL.createPixmapFont filePath
    -- the creation methods that "don't work" (maybe to make them work we need to use opengl matrix positionning?)
    --FTGL.createTextureFont filePath
    --FTGL.createBufferFont filePath
    --FTGL.createOutlineFont filePath
    --FTGL.createPolygonFont filePath
    --FTGL.createTextureFont filePath
    --FTGL.createExtrudeFont filePath

charsetAABB :: CharSet ForOffset -> FTGL.Font -> IO (AABB FontCoordinates)
charsetAABB (CharSet []) _ = error "empty charset"
charsetAABB (CharSet charset) font = do
  (errors, aabbs) <- partitionEithers <$> mapM (glyphBBox font) charset
  unless (null errors) $
    error $ show errors
  return $ fromMaybe (error "logic") $
    foldl' (\r aabb -> Just $ maybe aabb (combine aabb) r) Nothing aabbs

data FontCoordinates
data UnitRectangleCoordinates

data AABB a = AABB {
  _min, _max :: {-# UNPACK #-} !(Vec2 Pos)
} deriving (Generic, Show)

-- | returns the smallest 'AABB' containg the given 'AABB's.
combine :: AABB a -> AABB a -> AABB a
combine (AABB (Vec2 xmin  ymin ) (Vec2 xmax  ymax ))
        (AABB (Vec2 xmin' ymin') (Vec2 xmax' ymax')) =
  AABB (Vec2 (min xmin xmin') (min ymin ymin'))
       (Vec2 (max xmax xmax') (max ymax ymax'))

-- | @a@ is strictly contained in @b@ iff @margin a b > 0@
margin :: AABB a -> AABB a -> Float
margin (AABB (Vec2 xmin  ymin ) (Vec2 xmax  ymax ))
       (AABB (Vec2 xmin' ymin') (Vec2 xmax' ymax')) =
  min marginMins marginMaxs
 where
  marginMins = min (xmin - xmin') (ymin - ymin')
  marginMaxs = min (xmax' - xmax) (ymax' - ymax)

glyphBBox :: FTGL.Font -> Char -> IO (Either String (AABB FontCoordinates))
glyphBBox font c =
  FTGL.getFontBBox font [c] >>= \case
    [xmin,ymin,_,xmax,ymax,_] ->
      return $ Right $ AABB (Vec2 xmin ymin) (Vec2 xmax ymax)
    other -> return $ Left $ "invalid font bbox:" ++ show (other, c)


-- |
-- @
-- .----------------.
-- |                |
-- |   .--------.   |  < aabb y max
-- |   |=======,|   |
-- |   |     // |   |
-- |   |   //   |   |
-- |   | //     |   |
-- |   |/=======|   |
-- |   .--------.   |  < aabb y min
-- |R               |  < glyph y ref = 0
-- O----------------.
--  ^   ^       ^
--  |   |       aabb x max
--  |   aabb x min
--  glyph x ref = 0
--
-- /outer/ rectangle = unit rectangle (O is the origin of its coordinate system)
-- /inner/ rectangle = aabb (R is the origin of its coordinate system)
-- @
--
-- Given the drawing above, we want to determine the position of @R@, expressed in the
-- coordinate system of the unit rectangle, such that the aabb is centered in the unit rectangle:
--
-- @
-- ppuW - (Rx + aabbXMax) = Rx + aabbXMin  -- centered horizontally
-- ppuH - (Ry + aabbYMax) = Ry + aabbYMin  -- centered vertically
-- @
--
-- Hence,
--
-- @
-- Rx = (ppuW - aabbXMax - aabbXMin) / 2
-- Ry = (ppuH - aabbYMax - aabbYMin) / 2
-- @
--
-- where values on the right of @=@ are known:
--
-- * ppuH, ppuW are specified by command line arguments,
-- * aabb{X|Y}{Min|Max} are retrieved using @FTGL.getFontBBox@.
--
-- Note that this holds also if the aabb is bigger than the unit rectangle.
offsetToCenterAABB :: AABB FontCoordinates
                   -> PPU
                   -> (Vec2 Pos, AABB UnitRectangleCoordinates)
offsetToCenterAABB a@(AABB (Vec2 xmin ymin) (Vec2 xmax ymax)) (Size ppuH ppuW) =
  (offset, applyOffset offset a)
 where
  x = (fromIntegral ppuW - xmax - xmin) / 2
  y = (fromIntegral ppuH - ymax - ymin) / 2
  offset = Vec2 x y

applyOffset :: Vec2 Pos -> AABB FontCoordinates -> AABB UnitRectangleCoordinates
applyOffset v (AABB vmin vmax) =
  AABB (sumVec2d vmin v) (sumVec2d vmax v)

unitRectangleAABB :: PPU -> AABB UnitRectangleCoordinates
unitRectangleAABB (Size ppuH ppuW) =
  AABB (Vec2 0 0) $ Vec2 (fromIntegral ppuW) (fromIntegral ppuH)
