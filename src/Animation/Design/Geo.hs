{-# LANGUAGE NoImplicitPrelude #-}

module Animation.Design.Geo
    ( gravityExplosionPure
    , simpleExplosionPure
    , quantitativeExplosionPure
    , animateNumberPure
    ) where

import           Imajuscule.Prelude

import           Data.List( length )

import           Animation.Types
import           Geo( Coords
                    , bresenham
                    , bresenhamLength
                    , mkSegment
                    , polyExtremities
                    , translatedFullCircle
                    , translatedFullCircleFromQuarterArc
                    , parabola
                    , Vec2(..)
                    , pos2vec
                    , vec2coords )
import           Resample( resample )


gravityExplosionPure :: Vec2 -> Coords -> Frame -> [Coords]
gravityExplosionPure initialSpeed origin (Frame iteration) =
  let o = pos2vec origin
  in  [vec2coords $ parabola o initialSpeed iteration]

simpleExplosionPure :: Int -> Coords -> Frame -> [Coords]
simpleExplosionPure resolution center (Frame iteration) =
  let radius = fromIntegral iteration :: Float
      c = pos2vec center
  in map vec2coords $ translatedFullCircleFromQuarterArc c radius 0 resolution

quantitativeExplosionPure :: Int -> Coords -> Frame -> [Coords]
quantitativeExplosionPure number center (Frame iteration) =
  let numRand = 10 :: Int
      rnd = 2 :: Int -- TODO store the random number in the state of the animation
  -- rnd <- getStdRandom $ randomR (0,numRand-1)
      radius = fromIntegral iteration :: Float
      firstAngle = (fromIntegral rnd :: Float) * 2*pi / (fromIntegral numRand :: Float)
      c = pos2vec center
  in map vec2coords $ translatedFullCircle c radius firstAngle number

animateNumberPure :: Int -> Coords -> Frame -> [Coords]
animateNumberPure n center (Frame i) =
  let r = animateRadius (1 + quot i 2) n
  in if r <= 0
       then
         []
       else
         case n of
            1 -> simpleExplosionPure 8 center $ Frame r
            _ -> polygon n r center

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
         max 0 (2 * limit - i)

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
