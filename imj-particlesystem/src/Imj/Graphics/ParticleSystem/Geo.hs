{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Graphics.ParticleSystem.Geo
    ( particlesFreefall
    , gravityExplosionGeo
    , particlesExplosion
    , particlesPolygonExpandShrink
    , particlesLaser
    , explosion
    -- integrating speed and forces
    , gravityMotion
    , zeroForceMotion
    -- meta functions
    , particles
    ) where

import           Imj.Prelude

import           Data.Char( intToDigit )
import           Data.List( length )

import           Imj.GameItem.Weapon.Laser.Types
import           Imj.Graphics.Class.Positionable
import           Imj.Graphics.Font
import           Imj.Graphics.ParticleSystem.Design.Types
import           Imj.Physics.Continuous.Types

import           Imj.Geo.Continuous
import           Imj.Geo.Discrete
import           Imj.Iteration

-- | Note that the 'VecPosSpeed' parameter is unused.
particlesLaser :: LaserRay Actual
               -> Colorization
               -> VecPosSpeed
               -- ^ Unused, because the 'LaserRay' encodes the origin already
               -> Frame
               -> [Particle]
particlesLaser (LaserRay dir start len) color _ frame@(Frame i)
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
        glyph = gameGlyph $
          if i >= 2
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
        (\(particleIndex,p) ->
          Particle DontInteract (mkStaticVecPosSpeed $ pos2vec p) glyph (color frame particleIndex))
        $ zip [0..] points

-- | Gravity free-fall
particlesFreefall :: Float
                  -> CanInteract
                  -> Glyph
                  -> Colorization
                  -> VecPosSpeed
                  -- ^ Initial position and speed
                  -> Frame
                  -> [Particle]
particlesFreefall scaleSpeed =
  let adaptSpeed (VecPosSpeed pos speed) =
        VecPosSpeed pos $ scalarProd scaleSpeed speed
  in particles ((:[]) . adaptSpeed) gravityMotion

{- | A generic function to produce particles -}
particles :: (VecPosSpeed -> [VecPosSpeed])
          -- ^ Produces initial particles from the seed particle.
          -> (Frame -> VecPosSpeed -> VecPosSpeed)
          -- ^ Integrates the motion of a particle
          -> CanInteract
          -> Glyph
          -> Colorization
          -> VecPosSpeed
          -- ^ The seed particle
          -> Frame
          -> [Particle]
particles producePs moveP canInteract g color seedP frame =
  map
    (\(particleIndex,p) ->
        let newP = moveP frame p
        in Particle canInteract newP g (color frame particleIndex))
    $ zip [0..] $ producePs seedP

-- | Integrates the motion according to
-- <https://en.wikipedia.org/wiki/Newton%27s_laws_of_motion#Newton's_first_law Newton's first law>,
-- assuming no force is applied.
zeroForceMotion :: Frame
                -> VecPosSpeed
                -> VecPosSpeed
zeroForceMotion frame (VecPosSpeed pos speed) =
  VecPosSpeed (sumVec2d pos $ integrateVelocity frame speed) speed

-- | Integrates the motion according to
-- <https://en.wikipedia.org/wiki/Newton%27s_laws_of_motion#Newton's_first_law Newton's first law>,
-- with the effect of gravity.
gravityMotion :: Frame
              -> VecPosSpeed
              -> VecPosSpeed
gravityMotion frame ps =
  parabola ps frame

explosionByQuartArcs :: Int
                     -- ^ Number of points per quarter arc.
                     -> Float
                     -- ^ First angle
                     -> VecPosSpeed
                     -- ^ Seed particle (its speed is ignored)
                     -> [VecPosSpeed]
                     -- ^ Produced particles (they have the same position as
                     -- the seed particle, but their velocities
                     -- are set so as to produce an explosion-like motion)
explosionByQuartArcs resolution angle (VecPosSpeed center _) =
  let points = fullCircleFromQuarterArc 1 angle resolution
  in map (\(Vec2 dx dy) ->  VecPosSpeed center $ Vec2 dx dy) points

explosionByCircle :: Int
                  -- ^ Number of points per quarter arc.
                  -> Float
                  -- ^ First angle
                  -> VecPosSpeed
                  -- ^ Seed particle (its speed is ignored)
                  -> [VecPosSpeed]
                  -- ^ Produced particles (they have the same position as
                  -- the seed particle, but their velocities
                  -- are set so as to produce an explosion-like motion)
explosionByCircle resolution angle (VecPosSpeed center _) =
  let points = fullCircle 1 angle resolution
  in map (\(Vec2 dx dy) -> VecPosSpeed center $ Vec2 dx dy) points

explosion :: Int
          -- ^ Number of points per quarter arc.
          -> Float
          -- ^ First angle
          -> VecPosSpeed
          -- ^ Seed particle (its speed is ignored)
          -> [VecPosSpeed]
          -- ^ Produced particles (they have the same position as
explosion n =
  let (q,r) = quotRem n 4
  in if r == 0
      then
        explosionByQuartArcs q
      else
        explosionByCircle n

-- | Circular explosion
particlesExplosion :: Int
                   -- ^ The number of particles
                   -> Float
                   -- ^ Angle of the first particle.
                   -> CanInteract
                   -> Glyph
                   -> Colorization
                   -> VecPosSpeed
                   -- ^ Center
                   -> Frame
                   -> [Particle]
particlesExplosion resolution angle =
  particles (explosion resolution angle) zeroForceMotion


-- | Explosion where each particle is subject to gravity.
gravityExplosionGeo :: Int
                    -- ^ The number of points of the circle
                    -> Float
                    -- ^ First angle
                    -> CanInteract
                    -> Glyph
                    -> Colorization
                    -> VecPosSpeed
                    -- ^ Center
                    -> Frame
                    -> [Particle]
gravityExplosionGeo resolution angle =
  particles (explosion resolution angle) gravityMotion

-- | Expanding then shrinking geometric figure.
particlesPolygonExpandShrink :: Int
                             -- ^ number of extremities of the polygon (if 1, draw a circle instead)
                             -> Colorization
                             -> VecPosSpeed
                             -- ^ Center
                             -> Frame
                             -- ^ Used to compute the radius.
                             -> [Particle]
particlesPolygonExpandShrink n colorFunc c@(VecPosSpeed center _) (Frame i) =
  let r = animateRadius (quot i 2) n
      frame' = Frame r
      glyph = gameGlyph $ intToDigit n
  in if r < 0
       then
         []
       else
         case n of
            1 -> particlesExplosion 32 0 DontInteract glyph colorFunc c frame'
            _ -> map (\(particleIndex,p') ->
                        let p = mkStaticVecPosSpeed $ pos2vec p'
                        in Particle DontInteract p glyph (colorFunc frame' particleIndex) )
                    $ zip [0..] $ polygon n r center

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
  in if i < limit
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

-- | Applies bresenham line algorithm and resamples it
sampledBresenham :: Int -> Coords Pos -> Coords Pos -> [Coords Pos]
sampledBresenham nSamples start end =
  let seg = mkSegment start end
      l = countSegmentElements seg
      bres = bresenham seg
  in resampleWithExtremities bres (assert (l == length bres) l) nSamples
