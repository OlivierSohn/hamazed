{-# LANGUAGE NoImplicitPrelude #-}

module Animation.Design.Geo
    ( gravityExplosionPure
    , simpleExplosionPure
    , quantitativeExplosionPure
    , animateNumberPure
    , simpleLaserPure
    ) where

import           Imajuscule.Prelude

import           Data.Char( intToDigit )
import           Data.List( length )

import           Animation.Types
import           Geo
import           Laser.Types
import           Resample( resample )


-- | doesn't use the Coords parameter
simpleLaserPure :: LaserRay Actual -> Coords -> Frame -> ([Coords], Maybe Char)
simpleLaserPure (LaserRay dir (Ray seg)) _ (Frame i) =
  let (originalChar, replacementChar) =
        if dir == LEFT || dir == RIGHT
          then
            ('=','-')
          else
            ('|','.')
      char = if i>= 2 then replacementChar else originalChar
      points = if i >= 4
                 then
                   []
                 else
                   showSegment seg
  in (points, Just char)

gravityExplosionPure :: Vec2 -> Coords -> Frame -> ([Coords], Maybe Char)
gravityExplosionPure initialSpeed origin (Frame iteration) =
  let o = pos2vec origin
  in  ([vec2coords $ parabola o initialSpeed iteration], Nothing)

simpleExplosionPure :: Int -> Coords -> Frame -> ([Coords], Maybe Char)
simpleExplosionPure resolution center (Frame iteration) =
  let radius = fromIntegral iteration :: Float
      c = pos2vec center
  in (map vec2coords $ translatedFullCircleFromQuarterArc c radius 0 resolution, Nothing)

quantitativeExplosionPure :: Int -> Coords -> Frame -> ([Coords], Maybe Char)
quantitativeExplosionPure number center (Frame iteration) =
  let numRand = 10 :: Int
      rnd = 2 :: Int -- TODO store the random number in the state of the animation
  -- rnd <- getStdRandom $ randomR (0,numRand-1)
      radius = fromIntegral iteration :: Float
      firstAngle = (fromIntegral rnd :: Float) * 2*pi / (fromIntegral numRand :: Float)
      c = pos2vec center
  in (map vec2coords $ translatedFullCircle c radius firstAngle number, Nothing)

animateNumberPure :: Int -> Coords -> Frame -> ([Coords], Maybe Char)
animateNumberPure n center (Frame i) =
  let r = animateRadius (quot i 2) n
      points = if r < 0
       then
         []
       else
         case n of
            1 -> fst $ simpleExplosionPure 8 center $ Frame r
            _ -> polygon n r center
  in (points, Just $ intToDigit n)

polygon :: Int -> Int -> Coords -> [Coords]
polygon nSides radius center =
  let startAngle = if odd nSides then pi else pi/4.0
  in connect $ polyExtremities nSides center radius startAngle

animateRadius :: Int -> Int -> Int
animateRadius i nSides =
  let limit
          | nSides <= 4 = 5
          | nSides <= 6 = 7
          | otherwise   = 10
  in if i < limit
       then
         i
       else
         2 * limit - i

connect :: [Coords] -> [Coords]
connect []  = []
connect l@[_] = l
connect (a:rest@(b:_)) = connect2 a b ++ connect rest

connect2 :: Coords -> Coords -> [Coords]
connect2 start end =
  let numpoints = 80 -- more than 2 * (max height width of world) to avoid spaces
  in sampledBresenham numpoints start end

sampledBresenham :: Int -> Coords -> Coords -> [Coords]
sampledBresenham nSamples start end =
  let l = bresenhamLength start end
      seg = mkSegment start end
      bres = bresenham seg
  in resample bres (assert (l == length bres) l) nSamples
