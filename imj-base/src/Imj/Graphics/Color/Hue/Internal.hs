
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Imj.Graphics.Color.Hue.Internal
          ( mkIntensity
          , Intensity(..)
          , rotateHue'
          , hue'
          , countHuesOfSameIntensity'
          ) where

import           Imj.Prelude

import           Imj.Graphics.Color.Types


{-
[@saturated cycles@]

(Can be abbreviated @sc@ in this documentation. This notion is not, to my knowledge,
described in the litterature, but I find it useful.)

In the 6x6x6 rgb color cube, a saturated cycle of intensity x is a sequence of
adjacent colors whose max component is x, and that have at least one zero component:

@
r g b   hue, in Turns
-----   -------------
x 0 0   0
..
x x 0   1/6
..
0 x 0   2/6
..
0 x x   3/6
..
0 0 x   4/6
..
x 0 x   5/6
..
(go to the beginning)
@

in 6x6x6 rgb color space, there are 6 saturated cycles:

@
sc of intensity 5 : length 30 (5*6)
sc of intensity 4 : length 24 (4*6)
sc of intensity 3 : length 18 (3*6)
sc of intensity 2 : length 12 (2*6)
sc of intensity 1 : length  6 (1*6)
sc of intensity 0 : length  1 (1)    -- contains only the black color
                           --
                           91 saturated colors
@

In the rgb space, an observer placed at 10,10,10 and looking towards 0,0,0 will see the
saturated cycles as concentric hexagons.

Here they are represented in 2d, with slices of the rgb space orthogonal to the blue axis:

b = 0:

@

5 5 5 5 5 5 5
| 4 4 4 4 4 5
| 3 3 3 3 4 5
g 2 2 2 3 4 5
| 1 1 2 3 4 5
0 0 1 2 3 4 5
  0 -- r -- 5

@

b = 1:

@

5 5 . . . . .
  4 . . . . .
  3 . . . . .
  2 . . . . .
  1 . . . . .
0 1 1 2 3 4 5
  0         5

@

b = 5:

@

5 5 . . . . .
  5 . . . . .
  5 . . . . .
  5 . . . . .
  5 . . . . .
0 5 5 5 5 5 5
  0         5

@
-}
data SaturatedColor a = SaturatedColor {
    _intensity :: {-# UNPACK #-} !Intensity
  -- ^ intensity of the saturated cycle
  , _index :: {-# UNPACK #-} !Int
  {- ^ 0 for pure red, and increases with hue. The range of indices
   depends on the intensity (see 'perimeter').
  -}
} deriving(Generic, Show)


{-
grays (r=g=b) : 5 (note that 0,0,0 is sc of intensity 0)

all colors: 6*6*6 = 216

non saturated colors (r,g,b != 0) : 120
-}

mkBlackSaturated :: SaturatedColor a
mkBlackSaturated = SaturatedColor (mkIntensity 0) 0

newtype Intensity = Intensity Int
  deriving(Generic, Show)

mkIntensity :: Int -> Intensity
mkIntensity i
 | i >= 0 && i <= 5 = Intensity i
 | otherwise = error $ "invalid intensity :" ++ show i

{-# INLINE perimeter #-}
perimeter :: Intensity -> Int
perimeter (Intensity i)
  | i < 0 || i > 5 = error $ "invalid intensity :" ++ show i
  | i == 0 = 1
  | otherwise = 6 * i

{-# INLINE edgeLength #-}
edgeLength :: Intensity -> Int
edgeLength (Intensity i)
  | i == 0 = error "not applicable"
  | i < 0 || i > 5 = error $ "invalid intensity :" ++ show i
  | otherwise = i

{- | Using the conventions defined
<https://en.wikipedia.org/wiki/HSL_and_HSV here>,
expressing the hue in Turns (as defined
<https://en.wikipedia.org/wiki/Angular_unit here>):
@
red = 0

green = 1/3

blue = 2/3
@

Grays, white and black have no hue.
-}
{-# INLINE hue'' #-}
hue'' :: SaturatedColor a -> Maybe Float
hue'' (SaturatedColor (Intensity 0) _) = Nothing
hue'' (SaturatedColor i index) = Just $ indexToHue i index

{-# INLINE hue' #-}
hue' :: Xterm256Color a -> Maybe Float
hue' = hue'' . snd . decompose

countHuesOfSameIntensity' :: Xterm256Color a -> Int
countHuesOfSameIntensity' c =
  let (_, SaturatedColor i _) = decompose c
  in perimeter i

{-# INLINE indexToHue #-}
indexToHue :: Intensity -> Int -> Float
indexToHue i index =
  let l = perimeter i
  in fromIntegral index / fromIntegral l

{-# INLINE hueToIndex #-}
hueToIndex :: Intensity -> Float -> Int
hueToIndex intensity hueValue =
  let l = perimeter intensity
  in round (hueValue * fromIntegral l) `mod` l

{-# INLINE min3 #-}
{-# INLINE max3 #-}
min3, max3 :: Int -> Int -> Int -> Int
min3 x y z = min x $ min y z
max3 x y z = max x $ max y z

rotateHue' :: Float -> Xterm256Color a -> Xterm256Color a
rotateHue' deltaHue c =
  let (x, saturated) = decompose c
  in recompose x $ rotateHue'' deltaHue saturated

{-# INLINE decompose #-}
decompose :: Xterm256Color a -> (Gray, SaturatedColor a)
decompose c =
  (desaturated, saturated)
 where
  (desaturated, color) = splitGray c
  saturated = case color of
    GrayColor _ -> error "unexpected"
    RGBColor (RGB r g b) -> rgbToSaturated r g b

{-# INLINE recompose #-}
recompose :: Gray -> SaturatedColor a -> Xterm256Color a
recompose (Gray24 g) (SaturatedColor (Intensity 0) _) = GrayColor g
recompose (Gray24 _) _ = error "by design, shoudn't add gray24 to non black"
recompose (Gray6 gr) s =
  let (r,g,b) = saturatedToRgb s
  in RGBColor $ RGB (gr + r) (gr + g) (gr + b)

{-# INLINE saturatedToRgb #-}
saturatedToRgb :: SaturatedColor a -> (Int, Int, Int)
saturatedToRgb (SaturatedColor (Intensity 0) _) = (0,0,0)
saturatedToRgb s@(SaturatedColor intensity idx) =
  let l = edgeLength intensity
      (segmentIndex, x) = quotRem (assert (idx >= 0 && idx < 6*l) idx) (2*l)
      (a,b) = if x <= l
                then (l    , x)
                else (2*l-x, l)
  in case segmentIndex of
        0 -> (a,b,0)
        1 -> (0,a,b)
        2 -> (b,0,a)
        _ -> error $ "unexpected " ++ show (segmentIndex, s)

{-# INLINE rgbToSaturated #-}
rgbToSaturated :: Int -> Int -> Int -> SaturatedColor a
rgbToSaturated r g b = case max3 r g b of
  0 -> mkBlackSaturated
  i ->
    let intensity = mkIntensity i
        l = edgeLength intensity
        computeIndex x y z
          | z == 0 = Just $ if x == l
                              then
                                y
                              else
                                assert (y == l && x < l) $ 2*l-x
          | otherwise = Nothing
        index = fromMaybe (error "unexpected") $ computeIndex r g b
                                <|> fmap (2*l +) (computeIndex g b r)
                                <|> fmap (4*l +) (computeIndex b r g)
    in SaturatedColor intensity index

rotateHue'' :: Float -> SaturatedColor a -> SaturatedColor a
rotateHue'' _ c@(SaturatedColor (Intensity 0) _) = c
rotateHue'' deltaHue (SaturatedColor i index) =
  SaturatedColor i $ hueToIndex i $ deltaHue + indexToHue i index

data Gray =
    Gray24 {-# UNPACK #-} !Int
    -- ^ isomorphic to 'GrayColor'
  | Gray6 {-# UNPACK #-} !Int
  -- ^ isomorphic to @{'RGBColor' r g b | r=g=b}@
  deriving(Generic, Show)

splitGray :: Xterm256Color a -> (Gray, Xterm256Color a)
splitGray (GrayColor g) = (Gray24 g, RGBColor $ RGB 0 0 0)
splitGray (RGBColor (RGB r g b)) =
    let m = min3 r g b
        r' = r - m
        g' = g - m
        b' = b - m
    in (Gray6 m, RGBColor $ RGB r' g' b')
