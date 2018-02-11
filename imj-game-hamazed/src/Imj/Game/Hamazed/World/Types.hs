{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Imj.Game.Hamazed.World.Types
        ( WorldId(..)
        , WorldSpec(..)
        , WorldEssence(..)
        , World(..)
        , ParticleSystemKey(..)
        , WorldParameters(..)
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
import           Control.DeepSeq(NFData)
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


data WorldParameters = WorldParameters {
    getWorldShape :: !WorldShape
  , getWallDistrib :: !WallDistribution
} deriving(Generic)

instance Binary WorldParameters
instance NFData WorldParameters

data WorldShape = Square
                -- ^ Width = Height
                | Rectangle2x1
                -- ^ Width = 2 * Height
                deriving(Generic, Show)

instance Binary WorldShape
instance NFData WorldShape

-- | How should walls be created?
data WallDistribution = None
                      -- ^ No 'Wall's.
                      | Deterministic
                      -- ^ A Rectangular 'Wall' in the middle of the level.
                      | Random !RandomParameters
                      -- ^ 'Wall's are created with an algorithm involving random numbers.
                      deriving(Generic, Show)

instance Binary WallDistribution
instance NFData WallDistribution

data WorldSpec = WorldSpec {
    getLevelNumber :: !Int
  , getShipIds :: ![ShipId]
  , getWorldParams :: !WorldParameters
  , getWorldId' :: !(Maybe WorldId) -- Maybe because some 'WorldSpec' are created by the client, for initialization
} deriving(Generic)
instance Binary WorldSpec

data WorldEssence = WorldEssence {
    getNumbers :: ![Number]
  , getShips :: ![BattleShip]
  , getSpaceMatrix :: ![[Material]] -- TODO ByteString would use 3 * 64 times less memory
  , getWorldId :: !(Maybe WorldId)
} deriving(Generic, Show)
instance Binary WorldEssence

newtype WorldId = WorldId Int64
  deriving(Generic, Show, Binary, Enum, Eq, NFData)

data World = World {
    getWorldNumbers :: ![Number]
    -- ^ The remaining 'Number's (shot 'Number's are removed from the list)
  , getWorldShips :: ![BattleShip]
  , getWorldSpace :: !Space
    -- ^ The 'Space' in which 'BattleShip' and 'Number's evolve
  , getWorldRenderedSpace :: !RenderedSpace
  , getParticleSystems :: !(Map ParticleSystemKey (Prioritized ParticleSystem))
    -- ^ Animated particle systems, illustrating player actions and important game events.
  , getId :: !(Maybe WorldId)
} deriving (Generic)

newtype ParticleSystemKey = ParticleSystemKey Int
  deriving (Eq, Ord, Enum, Show, Num)

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
  , _shipArmor :: !Bool
  -- ^ True if ship has an armor protecting it.
  , getCollisions :: ![Number]
  -- ^ Which 'Number's are currently colliding with the 'BattleShip'.
} deriving(Generic, Show)
instance Binary BattleShip

newtype ShipId = ShipId Int64 deriving(Generic, Binary, Eq, Show, Enum, NFData)

-- This is good enough for now.
mkShipId :: IO ShipId
mkShipId = ShipId . toMicros . unsafeFromTimeSpec . unsafeGetTimeSpec <$> getSystemTime

data Number = Number {
    _numberPosSpeed :: !PosSpeed
  -- ^ Discrete position and speed.
  , getNumber :: !Int
  -- ^ Which number it represents (1 to 16).
} deriving(Generic, Eq, Show)

instance Binary Number

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
getWorldOffset mode (World _ ships space _ _ _) =
  case mode of
    CenterSpace -> zeroCoords
    CenterShip myId ->
      let worldCenter = Coords (fromIntegral $ quot h 2) (fromIntegral $ quot w 2)
          (Size h w) = getSize space
          (BattleShip _ (PosSpeed shipPos _) _ _ _) = findShip myId ships
      in diffCoords worldCenter shipPos

getWorldCorner :: World -> Coords Pos -> Coords Pos -> Coords Pos
getWorldCorner world screenCenter offset =
  let (h',w') = (quot h 2, quot w 2)
      (Size h w) = getSize $ getWorldSpace $ world
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
scopedLocation world@(World _ _ space _ _ _) mode (Screen mayTermSize screenCenter) scope pos =
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
  worldArea = mkRectArea zeroCoords $ getSize space
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
