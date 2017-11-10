
module Laser
    ( LaserType(..)
    , laserChar
    , shootLaserFromShip
    ) where


import           Geo( Coords(..)
                    , Direction(..)
                    , mkSegment
                    , Segment(..)
                    , translateCoord )
import           World( extend
                      , Location(..)
                      , location )

data LaserType = Infinite

shootLaserFromShip :: Coords -> Direction -> LaserType -> Maybe Segment
shootLaserFromShip shipCoords dir = shootLaser (translateCoord dir shipCoords) dir

shootLaser :: Coords -> Direction -> LaserType -> Maybe Segment
shootLaser laserStart dir laserType =
  case location laserStart of
    OutsideWorld -> Nothing
    InsideWorld ->
      case laserType of
        Infinite ->
          let laserEnd = extend laserStart dir
          in Just $ mkSegment laserStart laserEnd


laserChar :: Direction -> Char
laserChar dir = case dir of
  Up    -> '|'
  Down  -> '|'
  LEFT  -> '-'
  RIGHT -> '-'
