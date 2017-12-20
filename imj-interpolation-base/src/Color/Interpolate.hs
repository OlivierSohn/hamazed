-- | Functions to interpolate colors.

module Color.Interpolate
        ( bresenhamColor8Length
        , bresenhamColor8
        ) where

import           Color.Types

import           Geo.Discrete.Bresenham3

import           Util

-- Interpolations between 2 rgb or 2 grays are well-defined, whereas
--   other interpolations will error. To improve on this, we could define conversion
--   functions between different representations in the future.
-- | The two input 'Color8' are supposed to be both 'rgb' or both 'gray'.
{-# INLINABLE bresenhamColor8Length #-}
bresenhamColor8Length :: Color8 a -> Color8 a -> Int
bresenhamColor8Length c c'
  | c == c' = 1
  | otherwise =
      case (color8CodeToXterm256 c, color8CodeToXterm256 c') of
        (RGBColor rgb1, RGBColor rgb2) -> bresenhamRGBLength rgb1 rgb2
        (GrayColor g1, GrayColor g2) -> 1 + fromIntegral (abs (g2 - g1))
        colors -> error $ "cannot get length between colors " ++ show colors

-- | The two input 'Color8' are supposed to be both 'rgb' or both 'gray'.
{-# INLINABLE bresenhamColor8 #-}
bresenhamColor8 :: Color8 a -> Color8 a -> [Color8 a]
bresenhamColor8 c c'
  | c == c' = [c]
  | otherwise =
      case (color8CodeToXterm256 c, color8CodeToXterm256 c') of
        (RGBColor rgb1, RGBColor rgb2) ->
          map (xterm256ColorToCode . RGBColor) $ bresenhamRGB rgb1 rgb2
        (GrayColor g1, GrayColor g2) ->
          map Color8 $ range g1 g2
        colors -> error $ "cannot interpolate between colors " ++ show colors

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
