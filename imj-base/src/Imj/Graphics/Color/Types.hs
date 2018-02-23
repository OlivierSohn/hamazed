{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Imj.Graphics.Color.Types
          ( Color8 -- constructor is intentionaly not exposed.
          , mkColor8
          , Background
          , Foreground
          , LayeredColor(..)
          , encodeColors
          , color8BgSGRToCode
          , color8FgSGRToCode
          , Word8
          , bresenhamColor8
          , bresenhamColor8Length
          , rgb
          , userRgb
          , gray
          , userGray
          , grayToRGB
          , color8ToUnitRGB
          , Xterm256Color(..)
          , color8CodeToXterm256
          , onBlack
          , whiteOnBlack
          , white, black, red, green, magenta, cyan, yellow, blue
          , RGB(..)
          ) where

import           Imj.Prelude

import           Data.Bits(shiftL, (.|.))
import           Data.Text(pack)
import           Data.Word (Word8, Word16)

import           Imj.Geo.Discrete.Bresenham3
import           Imj.Graphics.Class.DiscreteDistance
import           Imj.Graphics.Class.DiscreteInterpolation
import           Imj.Util

-- | Components are expected to be between 0 and 5 included.
data RGB = RGB {
    _rgbR :: {-# UNPACK #-} !Word8
  , _rgbG :: {-# UNPACK #-} !Word8
  , _rgbB :: {-# UNPACK #-} !Word8
} deriving(Eq, Show, Read, PrettyVal, Generic)

-- | A background and a foreground 'Color8'.
data LayeredColor = LayeredColor {
    _colorsBackground :: {-# UNPACK #-} !(Color8 Background)
  , _colorsForeground :: {-# UNPACK #-} !(Color8 Foreground)
} deriving(Eq, Show, PrettyVal, Generic)

-- TODO use bresenham 6 to interpolate foreground and background at the same time:
-- https://nenadsprojects.wordpress.com/2014/08/08/multi-dimensional-bresenham-line-in-c/
-- | First interpolate background color, then foreground color
instance DiscreteDistance LayeredColor where
  distance (LayeredColor bg fg) (LayeredColor bg' fg') =
    succ $ pred (distance bg bg') + pred (distance fg fg')

-- | First interpolate background color, then foreground color
instance DiscreteInterpolation LayeredColor where
  interpolate (LayeredColor bg fg) (LayeredColor bg' fg') i
    | i < lastBgFrame = LayeredColor (interpolate bg bg' i) fg
    | otherwise       = LayeredColor bg' $ interpolate fg fg' $ i - lastBgFrame
    where
      lastBgFrame = pred $ distance bg bg'


{-# INLINE encodeColors #-}
encodeColors :: LayeredColor -> Word16
encodeColors (LayeredColor (Color8 bg') (Color8 fg')) =
  let fg = fromIntegral fg' :: Word16
      bg = fromIntegral bg' :: Word16
  in (bg `shiftL` 8) .|. fg


-- | Creates a rgb 'Color8' as defined in
-- <https://en.wikipedia.org/wiki/ANSI_escape_code#8-bit ANSI 8-bit colors>
--
-- Input components are expected to be in range [0..5].
-- See 'userRgb' for a version that returns an error when the input is out of range.
rgb :: Word8
    -- ^ red component in [0..5]
    -> Word8
    -- ^ green component in [0..5]
    -> Word8
    -- ^ blue component in [0..5]
    -> Color8 a
rgb r g b
  | r >= 6 || g >= 6 || b >= 6 = error "out of range"
  | otherwise = unsafeRgb r g b

-- | Same as 'rgb', but returns an error if the input value is not in the admissible range.
userRgb :: Word8 -> Word8 -> Word8 -> Either Text (Color8 a)
userRgb r g b =
  maybe
    (Right $ unsafeRgb r g b)
    Left
    $ check r <|> check g <|> check b
 where
  check x
   | x > 5 = Just $ "rgb color components must be in the [0,5] range. '" <> pack (show x) <> "' is out of range."
   | otherwise = Nothing

{-# INLINE unsafeRgb #-}
unsafeRgb :: Word8
          -- ^ red component in [0..5]
          -> Word8
          -- ^ green component in [0..5]
          -> Word8
          -- ^ blue component in [0..5]
          -> Color8 a
unsafeRgb r g b = Color8 $ fromIntegral $ 16 + 36 * r + 6 * g + b


-- | Creates a gray 'Color8' as defined in
-- <https://en.wikipedia.org/wiki/ANSI_escape_code#8-bit ANSI 8-bit colors>
--
-- Input is expected to be in the range [0..23] (from darkest to lightest)
gray :: Word8
     -- ^ gray value in [0..23]
     -> Color8 a
gray i
  | i >= 24 = error "out of range gray"
  | otherwise      = unsafeGray i

userGray :: Word8
         -- ^ gray value in [0..23]
         -> Either Text (Color8 a)
userGray i
  | i >= 24   = Left $ "gray color must be in the [0,23] range. '" <> pack (show i) <> "' is out of range."
  | otherwise = Right $ unsafeGray i

{-# INLINE unsafeGray #-}
unsafeGray :: Word8
           -- ^ gray value in [0..23]
           -> Color8 a
unsafeGray i = Color8 $ fromIntegral (i + 232)


data Foreground
data Background
-- | ANSI allows for a palette of up to 256 8-bit colors.
newtype Color8 a = Color8 Word8 deriving (Eq, Show, Read, Enum, Generic)
instance PrettyVal (Color8 a)
instance Binary (Color8 a)

-- | Using bresenham 3D algorithm in RGB space.
instance DiscreteDistance (Color8 a) where
  distance = bresenhamColor8Length

-- | Using bresenham 3D algorithm in RGB space.
instance DiscreteInterpolation (Color8 a) where
  interpolate c c' i
    | i >= lastFrame = c'
    | otherwise = bresenhamColor8 c c' !! clamp i 0 lastFrame
    where
      !lastFrame = pred $ bresenhamColor8Length c c'


{-# INLINE mkColor8 #-}
mkColor8 :: Word8 -> Color8 a
mkColor8 = Color8

-- | Converts a 'Color8' 'Foreground' to corresponding
-- <https://vt100.net/docs/vt510-rm/SGR.html SGR codes>.
color8FgSGRToCode :: Color8 Foreground -> [Int]
color8FgSGRToCode (Color8 c) =
  [38, 5, fromIntegral c]

-- | Converts a 'Color8' 'Background' to corresponding
-- <https://vt100.net/docs/vt510-rm/SGR.html SGR codes>.
color8BgSGRToCode :: Color8 Background -> [Int]
color8BgSGRToCode (Color8 c) =
  [48, 5, fromIntegral c]

-- | Computes the bresenham length between two colors. If both are 'gray', the
-- interpolation happens in grayscale space.
{-# INLINABLE bresenhamColor8Length #-}
bresenhamColor8Length :: Color8 a -> Color8 a -> Int
bresenhamColor8Length !c !c'
  | c == c' = 1
  | otherwise = case (color8CodeToXterm256 c, color8CodeToXterm256 c') of
      (GrayColor g1,  GrayColor g2)  -> 1 + fromIntegral (abs (g2 - g1))
      (RGBColor rgb1, RGBColor rgb2) -> bresenhamRGBLength rgb1 rgb2
      (RGBColor rgb1, GrayColor g2)  -> bresenhamRGBLength rgb1 (grayToRGB rgb1 g2)
      (GrayColor g1,  RGBColor rgb2) -> bresenhamRGBLength (grayToRGB rgb2 g1) rgb2

{- | Returns the bresenham path between two colors.

If both are 'gray', the interpolation happens in grayscale space.

If one is a 'gray' and the other 'rgb', the 'gray' one will be approximated by
the closest 'rgb' in the direction of the other color, so as to produce
a /monotonic/ interpolation. -}
{-# INLINABLE bresenhamColor8 #-}
bresenhamColor8 :: Color8 a -> Color8 a -> [Color8 a]
bresenhamColor8 !c !c'
  | c == c' = [c]
  | otherwise = case (color8CodeToXterm256 c, color8CodeToXterm256 c') of
    (GrayColor g1, GrayColor g2)   -> map gray $ range g1 g2
    (RGBColor rgb1, RGBColor rgb2) -> mapBresRGB rgb1 rgb2
    (RGBColor rgb1, GrayColor g2)  -> mapBresRGB rgb1 (grayToRGB rgb1 g2)
    (GrayColor g1, RGBColor rgb2)  -> mapBresRGB (grayToRGB rgb2 g1) rgb2
  where
    mapBresRGB c1 c2 = map (xterm256ColorToCode . RGBColor) $ bresenhamRGB c1 c2

{-# INLINABLE bresenhamRGBLength #-}
bresenhamRGBLength :: RGB -> RGB -> Int
bresenhamRGBLength (RGB r g b) (RGB r' g' b') =
  bresenham3Length (fromIntegral r,fromIntegral g,fromIntegral b) (fromIntegral r',fromIntegral g',fromIntegral b')

{-# INLINABLE bresenhamRGB #-}
bresenhamRGB :: RGB -> RGB -> [RGB]
bresenhamRGB (RGB r g b) (RGB r' g' b') =
  map
    (\(x,y,z) -> RGB (fromIntegral x) (fromIntegral y) (fromIntegral z))
    $ bresenham3 (fromIntegral r ,fromIntegral g ,fromIntegral b )
                 (fromIntegral r',fromIntegral g',fromIntegral b')


-- | Converts a 'Color8' to a 'Xterm256Color'.
color8CodeToXterm256 :: Color8 a -> Xterm256Color a
color8CodeToXterm256 (Color8 c)
  | c < 16    = error "interpolating 4-bit system colors is not supported" -- 4-bit ANSI color
  | c < 232   = RGBColor $ asRGB (c - 16)   -- interpreted as 8-bit rgb
  | otherwise = GrayColor (c - 232)         -- interpreted as 8-bit grayscale
 where
  asRGB i = let -- we know that i = 36 × r + 6 × g + b and (0 ≤ r, g, b ≤ 5)
                -- (cf. comment on top) so we can deduce the unique set of
                -- corresponding r g and b values:
                r = quot i 36
                g = quot (i - 36 * r) 6
                b = i - (6 * g + 36 * r)
            in  RGB r g b

-- For safety the values of RGBColor and GrayColor are clamped to their respective ranges.
-- | Converts a 'Xterm256Color' to a 'Color8'.
xterm256ColorToCode :: Xterm256Color a -> Color8 a
-- 8-bit rgb colors are represented by code:
-- 16 + 36 × r + 6 × g + b (0 ≤ r, g, b ≤ 5) (see link to spec above)
xterm256ColorToCode (RGBColor (RGB r' g' b'))
  = Color8 (16 + 36 * r + 6 * g + b)
  where
    clamp' x = clamp x 0 5
    r = clamp' r'
    g = clamp' g'
    b = clamp' b'
-- 8-bit grayscale colors are represented by code: 232 + g (g in [0..23]) (see
-- link to spec above)
xterm256ColorToCode (GrayColor y) = Color8 (232 + clamp y 0 23)

-- | Represents the rgb and grayscale xterm 256 colors
--
--  The ranges of colors that can be represented by each constructor are specified
--  <https://en.wikipedia.org/wiki/ANSI_escape_code#Colors here>.
data Xterm256Color a = RGBColor !RGB
                     -- ^ corresponding ANSI range:
                     --
                     -- - [0x10-0xE7]:  6 × 6 × 6 cube (216 colors):
                     --             16 + 36 × r + 6 × g + b (0 ≤ r, g, b ≤ 5)
                     | GrayColor !Word8
                     -- ^ corresponding ANSI range:
                     --
                     -- - [0xE8-0xFF]:  grayscale from dark gray to near white in 24 steps
                     deriving (Eq, Show, Read)


color8ToUnitRGB :: Color8 a -> (Float, Float, Float)
color8ToUnitRGB c =
  case color8CodeToXterm256 c of
    RGBColor (RGB r g b) -> (rgbToUnit r,rgbToUnit g,rgbToUnit b)
    GrayColor g -> let v = grayToUnit g in (v,v,v)
  where
    rgbToUnit :: Word8 -> Float
    rgbToUnit x = fromIntegral (xtermMapRGB8bitComponent x) / 255
    grayToUnit :: Word8 -> Float
    grayToUnit gr = fromIntegral (xtermMapGray8bitComponent gr) / 255


-- | how xterm interprets 8bit rgb colors (deduced from https://jonasjacek.github.io/colors/)
xtermMapRGB8bitComponent :: Word8
                         -- ^ input values are in range [0..5]
                         -- (the admissible range for rgb components of 8 bit
                         -- rgb ANSI colors, cf.
                         -- https://en.wikipedia.org/wiki/ANSI_escape_code#Colors)
                         -> Word8
                         -- ^ output is in range [0..255]
xtermMapRGB8bitComponent 0 = 0
xtermMapRGB8bitComponent n = 55 + n * 40

-- | how xterm interprets 8bit grayscale colors (deduced from https://jonasjacek.github.io/colors/)
xtermMapGray8bitComponent :: Word8
                         -- ^ input values are in range [0..23]
                         -- (the admissible range for gray component of 8 bit
                         -- grayscale ANSI colors, cf.
                         -- https://en.wikipedia.org/wiki/ANSI_escape_code#Colors)
                          -> Word8
                          -- ^ output is in range [0..255]
xtermMapGray8bitComponent v = 8 + 10 * v

{-# INLINE onBlack #-}
-- | Creates a 'LayeredColor' with a black background color.
onBlack :: Color8 Foreground -> LayeredColor
onBlack = LayeredColor (rgb 0 0 0)

{-# INLINE whiteOnBlack #-}
-- | Creates a 'LayeredColor' with white foreground and black background color.
whiteOnBlack :: LayeredColor
whiteOnBlack = onBlack white

red, green, blue, yellow, magenta, cyan, white, black :: Color8 a
red     = rgb 5 0 0
green   = rgb 0 5 0
blue    = rgb 0 0 5
yellow  = rgb 5 5 0
magenta = rgb 5 0 5
cyan    = rgb 0 5 5
white   = rgb 5 5 5
black   = rgb 0 0 0



-- | converts a GrayColor to the closest RGBColor, using another RGBColor
-- to know in which way to approximate.
grayToRGB :: RGB
          -- ^ We'll round the resulting r,g,b components towards this color
          -> Word8
          -- ^ The gray component
          -> RGB
grayToRGB (RGB r g b) grayComponent =
  RGB (approximateGrayComponentAsRGBComponent r grayComponent)
      (approximateGrayComponentAsRGBComponent g grayComponent)
      (approximateGrayComponentAsRGBComponent b grayComponent)


approximateGrayComponentAsRGBComponent :: Word8
                                       -- ^ rgb target component to know in which way to appoximate
                                       -> Word8
                                       -- ^ gray component
                                       -> Word8
                                       -- ^ rgb component
approximateGrayComponentAsRGBComponent _ 0 = 0
approximateGrayComponentAsRGBComponent _ 1 = 0
approximateGrayComponentAsRGBComponent colorComponent grayComponent =
  let c = grayComponentToFollowingRGBComponent grayComponent
  in if colorComponent < c
       then
         -- using the /following/ component, we went in the wrong direction
         -- so we take the previous one.
         pred c
       else
         -- using the /following/ component, we went in the right direction
         c

{- Gives the first RGB component that has a color value strictly greater
than the color value of the gray component.

Using implementations of 'xtermMapGray8bitComponent' and 'xtermMapRGB8bitComponent'
we can deduce the following correspondances, where we align values:
@
rgb  val:  0         95      135       175       215       255
rgb  idx:  0         1        2         3         4         5
gray idx:   0 1    8  9    12  13    16  17    20  21    23
gray val:   8 18.. 88 98.. 128 138.. 168 178.. 208 218.. 238
@

Note that no 2 color values match between rgb and gray.
-}
grayComponentToFollowingRGBComponent :: Word8
                                      -- ^ gray component
                                      -> Word8
                                      -- ^ rgb component
grayComponentToFollowingRGBComponent g
  | g > 20 = 5
  | g > 16 = 4
  | g > 12 = 3
  | g > 8  = 2
  | otherwise = 1
