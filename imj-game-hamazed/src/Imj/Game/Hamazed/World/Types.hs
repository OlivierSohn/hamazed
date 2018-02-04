{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Imj.Game.Hamazed.World.Types
        ( World(..)
        , ParticleSystemKey(..)
        , startWorld
        , WallDistribution(..)
        , WorldShape(..)
        , BattleShip(..)
        , mkShipId
        , findShip
        , partitionShips
        , ShipId(..)
        , Number(..)
        , Scope(..)
        , ViewMode(..)
        , Screen(..)
        , getColliding
        , computeViewDistances
        , getWorldCorner
        , getWorldOffset
        , envDistance
        , environmentInteraction
        , scopedLocation
        , mkScreen
        -- * Reexports
        , module Imj.Iteration
        , module Imj.Graphics.Text.Animation
        , module Imj.Physics.Discrete.Types
        , Terminal.Window
        , RectContainer(..)
        , RectArea, Filter, Positive
        ) where

import           Imj.Prelude

import qualified System.Console.Terminal.Size as Terminal(Window(..))

import           Data.List(find, partition)
import           Data.Map.Strict(Map)

import           Imj.Game.Hamazed.Loop.Event.Priorities
import           Imj.Game.Hamazed.World.Space.Types
import           Imj.Game.Hamazed.World.Space
import           Imj.Geo.Continuous.Types
import           Imj.Geo.Discrete
import           Imj.Graphics.ParticleSystem.Design.Types
import           Imj.Graphics.Text.Animation
import           Imj.Graphics.UI.RectArea
import           Imj.Graphics.UI.RectContainer
import           Imj.Iteration
import           Imj.Physics.Discrete
import           Imj.Physics.Discrete.Types
import           Imj.Timing


data WorldShape = Square
                -- ^ Width = Height
                | Rectangle2x1
                -- ^ Width = 2 * Height

-- | How should walls be created?
data WallDistribution = None
                      -- ^ No 'Wall's.
                      | Deterministic
                      -- ^ A Rectangular 'Wall' in the middle of the level.
                      | Random !RandomParameters
                      -- ^ 'Wall's are created with an algorithm involving random numbers.

data World = World {
    _worldNumbers :: ![Number]
    -- ^ The remaining 'Number's (shot 'Number's are removed from the list)
  , _worldShips :: ![BattleShip]
  , _worldSpace :: !Space
    -- ^ The 'Space' in which 'BattleShip' and 'Number's evolve
  , _worldParticleSystems :: !(Map ParticleSystemKey (Prioritized ParticleSystem))
    -- ^ Animated particle systems, illustrating player actions and important game events.
}

newtype ParticleSystemKey = ParticleSystemKey Int
  deriving (Eq, Ord, Enum, Show, Num)

startWorld :: Time Point System -> World -> World
startWorld t (World a ships b c) =
  let setSafeTime (BattleShip sid ship ammo _ col) = BattleShip sid ship ammo (Just $ addDuration (fromSecs 5) t) col
  in World a (map setSafeTime ships) b c


data ViewMode = CenterShip ShipId
              -- ^ the 'BattleShip' position is fixed w.r.t the screen.
              | CenterSpace
              -- ^ the 'Space' frame is fixed w.r.t the screen.
              deriving(Show)

computeViewDistances :: ViewMode -> (Length Width, Length Height)
computeViewDistances (CenterShip _) = (30, 2) -- it will overlapp for large worlds but that's okay:
                                          -- the overlap is far away from where the ship is
computeViewDistances CenterSpace = (20, 2)

data BattleShip = BattleShip {
    getShipId :: !ShipId
  , _shipPosSpeed :: !PosSpeed
  -- ^ Discrete position and speed.
  , getAmmo :: !Int
  -- ^ How many laser shots are left.
  , _shipSafeUntil :: !(Maybe (Time Point System))
  -- ^ At the beginning of each level, the ship is immune to collisions with 'Number's
  -- for a given time. This field holds the time at which the immunity ends.
  , getCollisions :: ![Number]
  -- ^ Which 'Number's are currently colliding with the 'BattleShip'.
} deriving(Show)

newtype ShipId = ShipId Int64 deriving(Generic, Binary, Eq, Show)

-- This is good enough for now.
mkShipId :: IO ShipId
mkShipId = ShipId . toMicros . unsafeFromTimeSpec . unsafeGetTimeSpec <$> getSystemTime

data Number = Number {
    _numberPosSpeed :: !PosSpeed
  -- ^ Discrete position and speed.
  , getNumber :: !Int
  -- ^ Which number it represents (1 to 16).
} deriving(Eq, Show)


getColliding :: Coords Pos -> [Number] -> [Number]
getColliding pos =
  filter (\(Number (PosSpeed pos' _) _) -> pos == pos')

envDistance :: Vec2 Pos -> Distance
envDistance (Vec2 x y) =
  if abs x > 500 || abs y > 500 -- TODO deduce it from the size of the terminal.
    then
      TooFar
    else
      DistanceOK

getWorldOffset :: ViewMode -> World -> Coords Pos
getWorldOffset mode (World _ ships (Space _ (Size h w) _) _) =
  case mode of
    CenterSpace -> zeroCoords
    CenterShip myId ->
      let worldCenter = Coords (fromIntegral $ quot h 2) (fromIntegral $ quot w 2)
          (BattleShip _ (PosSpeed shipPos _) _ _ _) = findShip myId ships
      in diffCoords worldCenter shipPos

getWorldCorner :: World -> Coords Pos -> Coords Pos -> Coords Pos
getWorldCorner (World _ _ (Space _ (Size h w) _) _) screenCenter offset =
  let (h',w') = (quot h 2, quot w 2)
  in sumCoords offset $ translate' (-h') (-w') screenCenter


-- | An interaction function taking into account a 'World' and 'Scope'
environmentInteraction :: World
                       -> ViewMode
                       -> Screen
                       -> Scope
                       -> Coords Pos
                       -> InteractionResult
environmentInteraction world mode screen scope =
  scopedLocation world mode screen scope >>> \case
    InsideWorld  -> Stable
    OutsideWorld -> Mutation

scopedLocation :: World
               -> ViewMode
               -> Screen
               -> Scope
               -- ^ The scope
               -> Coords Pos
               -- ^ The coordinates to test
               -> Location
scopedLocation world@(World _ _ space@(Space _ sz _) _) mode (Screen mayTermSize screenCenter) scope pos =
  let termContains (Size h w) =
        let corner = getWorldCorner world screenCenter $ getWorldOffset mode world
            (Coords r c) = sumCoords pos corner
        in r < fromIntegral h && c >= 0 && c < fromIntegral w
  in case scope of
    WorldScope mat -> if worldArea `contains` pos && mat == unsafeGetMaterial pos space
                        then
                          InsideWorld
                        else
                          OutsideWorld
    NegativeWorldContainer -> if worldViewArea `contains` pos
                                then
                                  OutsideWorld
                                else
                                  maybe
                                    InsideWorld
                                    (\szTerm ->
                                      if termContains szTerm
                                        then
                                          InsideWorld
                                        else
                                          OutsideWorld)
                                      mayTermSize
 where
  worldArea = mkRectArea zeroCoords sz
  worldViewArea = growRectArea 1 worldArea


data Screen = Screen {
    _screenSize :: !(Maybe Size)
  -- ^ Maybe we couldn't get the screen size.
  , _screenCenter :: !(Coords Pos)
  -- ^ The center is deduced from screen size, if any, or guessed.
}


mkScreen :: Maybe Size -> Screen
mkScreen sz =
  let center = maybe
                (Coords 40 80)
                (\(Size h w) -> Coords (fromIntegral $ quot h 2)
                                       (fromIntegral $ quot w 2))
                  sz
  in Screen sz center

findShip :: ShipId -> [BattleShip] -> BattleShip
findShip withId =
  fromMaybe (error $ "ship not found : " ++ show withId)
  . find (\s -> getShipId s == withId)

partitionShips :: ShipId -> [BattleShip] -> (BattleShip, [BattleShip])
partitionShips onId ships =
  let (l1,l2) = partition (\s -> getShipId s == onId) ships
  in case l1 of
      [ship] -> (ship, l2)
      [] -> error $ "ship not found : " ++ show onId
      _ -> error $ "ship not unique : " ++ show (onId, l1)
