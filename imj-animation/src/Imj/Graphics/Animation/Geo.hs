{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Graphics.Animation.Geo
    ( gravityFallGeo
    , simpleExplosionGeo
    , quantitativeExplosionGeo
    , animatePolygonGeo
    , laserAnimationGeo
    ) where

import           Imj.Prelude

import           Data.Char( intToDigit )
import           Data.List( length )

import           Imj.GameItem.Weapon.Laser.Types
import           Imj.Geo.Continuous
import           Imj.Geo.Discrete
import           Imj.Graphics.Animation.Design.Types
import           Imj.Iteration
import           Imj.Physics.Continuous.Types

-- | Note that the Coords parameter is unused.
laserAnimationGeo :: LaserRay Actual
                  -> VecPosSpeed
                  -- ^ Unused, because the 'LaserRay' encodes the origin already
                  -> Frame
                  -> [AnimatedPoint]
laserAnimationGeo (LaserRay dir start len) _ (Frame i)
  | len == 0 = []
  | otherwise =
    -- frame 0 and 1 : original char
    -- frame 2 and 3 : replacement char
    -- frame 4 : the end
    let (originalChar, replacementChar) =
          if dir == LEFT || dir == RIGHT
            then
              ('=','-')
            else
              ('|','.')
        char = if i >= 2
                  then
                    replacementChar
                  else
                    originalChar
        points = if i >= 4
                   then
                     []
                   else
                     map (\n -> move n dir start) [0..fromIntegral $ pred len]
    in map
        (\p -> AnimatedPoint DontInteract (mkStaticVecPosSpeed $ pos2vec p) (Just char) Nothing)
        points

-- | Gravity free-fall
gravityFallGeo :: Float
               -> CanInteract
               -> (Frame -> LayeredColor)
               -> VecPosSpeed
               -- ^ Initial position and speed
               -> Frame
               -> [AnimatedPoint]
gravityFallGeo scaleSpeed canInteract colorFunc (VecPosSpeed pos speed) frame =
  let point = parabola (VecPosSpeed pos $ scalarProd scaleSpeed speed) frame
  in [AnimatedPoint canInteract point Nothing (Just $ colorFunc frame)]

-- | Circular explosion by copying quarter arcs.
simpleExplosionGeo :: Int
                   -- ^ Number of points per quarter arc.
                   -> CanInteract
                   -> (Frame -> LayeredColor)
                   -> VecPosSpeed
                   -- ^ Center
                   -> Frame
                   -> [AnimatedPoint]
simpleExplosionGeo resolution canInteract colorFunc (VecPosSpeed center _) frame@(Frame iteration) =
  let radius = fromIntegral iteration :: Float
      points = translatedFullCircleFromQuarterArc center radius 0 resolution
  in map (\p -> AnimatedPoint canInteract (mkStaticVecPosSpeed p) Nothing (Just $ colorFunc frame)) points

-- | Circular explosion
quantitativeExplosionGeo :: Int
                         -- ^ The number of points of the circle
                         -> CanInteract
                         -> VecPosSpeed
                         -- ^ Center
                         -> Frame
                         -> [AnimatedPoint]
quantitativeExplosionGeo number canInteract (VecPosSpeed center _) (Frame iteration) =
  let numRand = 10 :: Int
      rnd = 2 :: Int -- TODO store the random number in the state of the animation
  -- rnd <- getStdRandom $ randomR (0,numRand-1)
      radius = fromIntegral iteration :: Float
      firstAngle = (fromIntegral rnd :: Float) * 2*pi / (fromIntegral numRand :: Float)
      points = translatedFullCircle center radius firstAngle number
  in map (\p -> AnimatedPoint canInteract (mkStaticVecPosSpeed p) Nothing Nothing) points

-- | Expanding then shrinking geometric figure.
animatePolygonGeo :: Int
                  -- ^ number of extremities of the polygon (if 1, draw a circle instead)
                  -> (Frame -> LayeredColor)
                  -> VecPosSpeed
                  -- ^ Center
                  -> Frame
                  -- ^ Used to compute the radius.
                  -> [AnimatedPoint]
animatePolygonGeo n colorFunc c@(VecPosSpeed center _) (Frame i) =
  let r = animateRadius (quot i 2) n
      frame' = Frame r
      points = if r < 0
       then
         []
       else
         case n of
            1 -> let p = simpleExplosionGeo 8 DontInteract colorFunc c frame'
                 in map (\(AnimatedPoint _ p' _ _) -> p') p
            _ -> map (mkStaticVecPosSpeed . pos2vec) $ polygon n r center
  in map (\p -> AnimatedPoint DontInteract p (Just $ intToDigit n) (Just $ colorFunc frame')) points

-- | A polygon using resampled bresenham to augment the number of points :
-- the number of points needs to be constant across the entire animation
-- so we need to resampleWithExtremities according to the biggest possible figure.
polygon :: Int -> Int -> Vec2 Pos -> [Coords Pos]
polygon nSides radius center =
  let startAngle = if odd nSides then pi else pi/4.0
      extrs = polyExtremities center (fromIntegral radius) startAngle nSides
  in connect $ map vec2pos extrs

-- | Animates the radius by first expanding then shrinking.
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

connect :: [Coords Pos] -> [Coords Pos]
connect []  = []
connect l@[_] = l
connect (a:rest@(b:_)) = connect2 a b ++ connect rest

connect2 :: Coords Pos -> Coords Pos -> [Coords Pos]
connect2 start end =
  let numpoints = 80 -- more than 2 * (max height width of world) to avoid spaces
  in sampledBresenham numpoints start end

-- | Applies bresenham transformation and resamples it
sampledBresenham :: Int -> Coords Pos -> Coords Pos -> [Coords Pos]
sampledBresenham nSamples start end =
  let l = bresenhamLength start end
      seg = mkSegment start end
      bres = bresenham seg
  in resampleWithExtremities bres (assert (l == length bres) l) nSamples
