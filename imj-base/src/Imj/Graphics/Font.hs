{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}

module Imj.Graphics.Font
    ( fontFiles
    , nFonts
    , withTempFontFile
    , Fonts(..)
    , createFonts
    , createFont
    , destroyUnusedFonts
    , applySizes
    , lookupFont
    , Font(..)
    , FontMargin(..)
    , Glyph(..)
    , decodeGlyph
    , textGlyph
    , gameGlyph
    , FontsVariations(..)
    , mkFontsVariations
    , FontVariations(..)
    , FontVariation(..)
    , updateVariations
    , updateVariation
    , getCurrentVariation
    , getFont
    , FontSpec(..)
    , CycleFont(..)
    , CycleFontSize(..)
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
import           Data.Map(Map)
import qualified Data.Map as Map(size, lookup, adjust, fromList)
import           Data.FileEmbed(embedFile)
import           Data.Word(Word32)
import           Foreign.Ptr(nullPtr)
import           System.IO.Temp(withSystemTempDirectory)

import qualified Graphics.Rendering.FTGL as FTGL

import           Imj.Geo.Continuous
import           Imj.Geo.Discrete.Types
import           Imj.Util

{-| These fonts have sometimes been modified to reduce the negative height
of the Pipe glyph, ascend the underline and center the '+'. -}
fontFiles :: [(ByteString, String, [(FontMargin, PPU)])]
fontFiles = [
             ($(embedFile "fonts/VCR_OSD_MONO_1.001.ttf"), "VCR",
            [(0, Size 12 8), (0, Size 13 9)])
           , ($(embedFile "fonts/Pixel LCD-7.ttf"), "LCD",
            [(0, Size 13 7), (0, Size 14 9), (0, Size 14 8), (0, Size 13 8), (0, Size 12 8)])
           , ($(embedFile "fonts/SrcCodPro-Bold-PipeReduced.ttf"), "SourceCodePro",
            [(0, Size 14 8), (0, Size 12 6)])
           , ($(embedFile "fonts/typwrng.ttf"), "Type wrong",
            [(0, Size 12 10)]) -- '| is offset'
           , ($(embedFile "fonts/whitrabt.ttf"), "White rabbit",
            [(0, Size 14 8), (1, Size 14 10)])
           , ($(embedFile "fonts/04B_30__.TTF"), "04B 30",
            [(0, Size 11 8)])
           , ($(embedFile "fonts/Extrude.ttf"), "Extrude",
            [(0, Size 15 13)])
           , ($(embedFile "fonts/3Dventure.ttf"), "3DVenture",
            [(0, Size 12 11)])
           ]

mkFontsVariations :: FontsVariations
mkFontsVariations = FontsVariations l 0
 where
  l = Map.fromList $
      zip [0..] $
      map
        (\(content, name, variations) ->
          FontVariations
            name
            content
            (Map.fromList $ zip [0..] $ map (uncurry $ flip FontVariation) variations)
            0)
            fontFiles

-- I want O(1) access to any font. For now this is ok, as I use 2 fonts only.
-- in the future, maybe use an "Immutable Storable Vector".
data Fonts = Fonts {
    getFont0 :: {-# UNPACK #-} !Font
  , getFont1 :: {-# UNPACK #-} !Font
} deriving (Generic, Show)
instance NFData Fonts

data Font = Font {
    getFTGLFont :: {-# UNPACK #-} !FTGL.Font
  , _fontSize :: {-# UNPACK #-} !FontSize
  -- ^ Should be equal to the last call to FTGL.getFontSize on this font
  , _offset :: {-# UNPACK #-} !(Vec2 Pos)
  -- ^ We use the /same/ offset for every character, to preserve font aspect.
} deriving(Generic, Show)
instance NFData Font

-- | Int passed to FTGL.setFontFaceSize
newtype FontSize = FontSize Int
  deriving(Generic, Show, NFData, Num, Integral, Real, Ord, Eq, Enum)

-- | (PPU = Pixels per unit). Defines the size of the rectangle, in pixels, allocated to a font character.
-- Horizontal and vertical dimensions should be even, because we divide them by 2
-- to draw numbers in binary representation.
type PPU = Size

-- | Distance between the enclosing aabb (for a given 'CharSet') and the unit rectangle (see 'PPU').
-- When positive, the charset is contained within the unit rectangle.
newtype FontMargin = FontMargin Float
  deriving(Ord, Eq, Generic, Show, NFData, Num)

newtype CycleFont = CycleFont Int
  deriving(Generic, Show, Integral, Num, Real, Ord, Eq, Enum, NFData)
newtype CycleFontSize = CycleFontSize Int
  deriving(Generic, Show, Integral, Num, Real, Ord, Eq, Enum, NFData)

data FontsVariations = FontsVariations {
    _catalog :: !(Map CycleFont FontVariations)
  , getCurrentFont :: !CycleFont
} deriving(Generic, Show)
instance NFData FontsVariations

data FontVariations = FontVariations {
    _fontName :: !String
  , _fontContent :: !ByteString
  , _ppuVariations :: !(Map CycleFontSize FontVariation)
  , _currentPPUVariation :: !CycleFontSize
} deriving(Generic, Show)
instance NFData FontVariations

data FontVariation = FontVariation {-# UNPACK #-} !PPU {-# UNPACK #-} !FontMargin
  deriving(Generic, Show)
instance NFData FontVariation

nFonts :: FontsVariations -> Int
nFonts (FontsVariations m _) = Map.size m

getFont :: CycleFont -> FontsVariations -> (ByteString, String)
getFont i (FontsVariations m _) = maybe
  (error $ "font lookup failed:" ++ show i)
  (\(FontVariations name content _ _) -> (content,name))
  $ Map.lookup i m

updateVariations :: CycleFont -> CycleFontSize -> FontsVariations -> FontsVariations
updateVariations f s (FontsVariations l cur)
  | sz == 0 = error "empty variations"
  | otherwise = FontsVariations newMap fontIdx
 where
   !sz = Map.size l
   fontIdx = (cur + f) `mod` fromIntegral sz
   newMap = Map.adjust (updateVariation s) fontIdx l

updateVariation :: CycleFontSize -> FontVariations -> FontVariations
updateVariation s (FontVariations content n variations cur)
  | sz == 0 = error "empty variation"
  | otherwise = FontVariations content n variations newIdx
 where
  sz = Map.size variations
  newIdx = (cur + s) `mod` fromIntegral sz

getCurrentVariation :: FontsVariations -> FontVariation
getCurrentVariation (FontsVariations l cur) =
  let (FontVariations _ _ var i) = fromMaybe (error $ "variation not found" ++ show cur) $ Map.lookup cur l
  in fromMaybe (error $ "variation not found" ++ show i) $ Map.lookup i var

newtype CharSet a = CharSet String
  deriving(Generic, Show)
data ForOffset

newtype Glyph = Glyph Word32
  deriving(Generic, Show, Eq)

-- | Defines the /kind/ of font (game font or text font)
newtype FontSpec = FontSpec Int
  deriving(Generic, Show)

applySizes :: Fonts -> IO ()
applySizes (Fonts f0 f1) = do
  applySize f0
  applySize f1

applySize :: Font -> IO ()
applySize (Font f size _) =
  setFontSize size f

showDetailed :: Font -> IO String
showDetailed ft@(Font f s _) = do
  details <- FTGL.getFontFaceSize f
  return $ show (ft, s, "size:", details)

destroyUnusedFonts :: Fonts -> Fonts -> IO ()
destroyUnusedFonts (Fonts (Font f0' _ _) (Font f1' _ _))
                   (Fonts (Font f0  _ _) (Font f1  _ _)) = do
  unless (f0' == f0 || f0' == f1) $ FTGL.destroyFont f0'
  unless (f1' == f0 || f1' == f1) $ FTGL.destroyFont f1'

lookupFont :: FontSpec -> Fonts -> Font
lookupFont (FontSpec 0) = getFont0
lookupFont (FontSpec 1) = getFont1
lookupFont (FontSpec n) = error $ "font index out of range : " ++ show n


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

mkUserPPU :: Length Width -> Length Height -> Either String PPU
mkUserPPU w h
  | w < 4 || h < 4 = Left $ "PPU values should be >= 4. At least one of them is too small:" ++ show (w,h)
  | odd w || odd h = Left $ "PPU values should be even. At least one of them is odd:" ++ show (w,h)
  | otherwise = Right $ Size h w

{-# INLINE half #-}
half :: PPU -> PPU
half (Size h w) = Size (quot h 2) (quot w 2)

floorToPPUMultiple :: Size -> PPU -> Size
floorToPPUMultiple (Size (Length h) (Length w)) (Size (Length ppuH) (Length ppuW)) =
  Size (fromIntegral $ f ppuH h)
       (fromIntegral $ f ppuW w)
 where
  f ppu l = ppu * quot l ppu

withTempFontFile :: ByteString -> String -> (String -> IO a) -> IO a
withTempFontFile content name act =
  withSystemTempDirectory "fontDir" $ \dir -> do
    let filePath = dir ++ "/font" ++ name ++ ".ttf"
    writeFile filePath content
    act filePath
    -- we don't delete the file, it will be done by 'withSystemTempDirectory'

createFonts :: (FontSpec -> CharSet ForOffset -> IO (Either String Font))
            -> IO (Either String Fonts)
createFonts mkFont = do
  gameFont <- mkFont (FontSpec 0) gameCharSet
  textFont <- mkFont (FontSpec 1) textCharSet
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

getFontSize :: FTGL.Font -> IO FontSize
getFontSize = fmap FontSize . FTGL.getFontFaceSize

setFontSize :: FontSize -> FTGL.Font -> IO ()
setFontSize x font =
  FTGL.setFontFaceSize font (fromIntegral x) 72 >>= \err -> do
    when (err /= 1) $ -- according to http://ftgl.sourceforge.net/docs/html/FTFont_8h.html#00c8f893cbeb98b663f9755647a34e8c
      error $ "setFontFaceSize failed: " ++ show (x, err)
    -- verify that we can reliably use 'FTGL.getFontFaceSize' as indicator of the last set size:
    s <- getFontSize font
    when (s /= x) $
      error $ "Font sizes mismatch: " ++ show (s, x)

createFont :: PPU -> FontMargin -> CharSet ForOffset -> FTGL.Font -> IO (Either String Font)
createFont ppu@(Size ppuH _) fontMargin charset font =
  if font == nullPtr
    then
      return $ Left "nullPtr"
  else do
    let maxFontSize = 2 * fromIntegral ppuH
        minFontSize = 1
        !rectAABB = unitRectangleAABB ppu
        setSize x = setFontSize x font
        condition x = do
          setSize x
          charsetAABB charset font >>= \aabb -> do
            let (_, offsetAABB) = offsetToCenterAABB aabb ppu
            -- return True if the translated bounding box of the character set is contained in the unit rectangle:
            return $ margin offsetAABB rectAABB >= fontMargin

    -- find the max font size such that the charset will be strictly contained
    -- in a unit rectangle.
    lastAbove False condition minFontSize maxFontSize >>= maybe
      (return $ Left $ "ppu is probably too small:" ++ show ppu)
      (\optimalSize -> do
          getFontSize font >>= \curSize ->
            -- maybe 'optimalSize' was not the last condition checked, in that case we set it again:
            when (curSize /= optimalSize) $ setSize optimalSize
          Right . Font font (fromIntegral optimalSize) . fst . flip offsetToCenterAABB ppu <$> charsetAABB charset font)


loadFont :: ByteString -> String -> IO FTGL.Font
loadFont b s = withTempFontFile b s $ \filePath ->
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
margin :: AABB a -> AABB a -> FontMargin
margin (AABB (Vec2 xmin  ymin ) (Vec2 xmax  ymax ))
       (AABB (Vec2 xmin' ymin') (Vec2 xmax' ymax')) =
  FontMargin $ min marginMins marginMaxs
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
  -- we use full pixel translations, as we use rasterized fonts:
  ix = round x :: Int
  iy = round y :: Int
  offset = Vec2 (fromIntegral ix) (fromIntegral iy)

applyOffset :: Vec2 Pos -> AABB FontCoordinates -> AABB UnitRectangleCoordinates
applyOffset v (AABB vmin vmax) =
  AABB (sumVec2d vmin v) (sumVec2d vmax v)

unitRectangleAABB :: PPU -> AABB UnitRectangleCoordinates
unitRectangleAABB (Size ppuH ppuW) =
  AABB (Vec2 0 0) $ Vec2 (fromIntegral ppuW) (fromIntegral ppuH)
