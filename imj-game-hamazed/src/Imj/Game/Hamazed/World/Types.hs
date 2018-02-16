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
        , findShip
        , ParticleSystemKey(..)
        , WorldParameters(..)
        , WallDistribution(..)
        , WorldShape(..)
        , BattleShip(..)
        , ShipId(..)
        , ShipStatus(..)
        , Number(..)
        , Scope(..)
        , ViewMode(..)
        , Screen(..)
        , ClientId(..)
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
import           Data.Map.Strict(Map, lookup)

import           Imj.Game.Hamazed.World.Space.Types
import           Imj.Geo.Continuous.Types
import           Imj.Graphics.ParticleSystem.Design.Types
import           Imj.Physics.Discrete.Types

import           Imj.Game.Hamazed.Chat
import           Imj.Game.Hamazed.Loop.Event.Priorities
import           Imj.Game.Hamazed.World.Space
import           Imj.Geo.Discrete
import           Imj.Graphics.Text.Animation
import           Imj.Graphics.UI.RectArea
import           Imj.Graphics.UI.RectContainer
import           Imj.Iteration
import           Imj.Physics.Discrete
import           Imj.Timing

data WorldParameters = WorldParameters {
    getWorldShape :: !WorldShape
  , getWallDistrib :: !WallDistribution
} deriving(Generic, Show)

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
    getLevelNumber :: {-# UNPACK #-} !Int
  , getShipIds :: {-# UNPACK #-} ![ClientId]
  , getWorldParams :: {-# UNPACK #-} !WorldParameters
  , getWorldId' :: {-# UNPACK #-} !(Maybe WorldId) -- Maybe because some 'WorldSpec' are created by the client, for initialization
} deriving(Generic, Show)
instance Binary WorldSpec

data ClientId = ClientId {
    getPlayerName :: {-# UNPACK #-} !PlayerName -- ^ primary key
  , getClientId :: {-# UNPACK #-} !ShipId -- ^ primary key
} deriving(Generic, Show)
instance NFData ClientId
instance Binary ClientId
-- | Match only on 'ShipId'.
instance Eq ClientId where
  x == y = (getClientId x) == (getClientId y)
  {-# INLINABLE (==) #-}

data WorldEssence = WorldEssence {
    getNumbers :: ![Number]
  , getShips :: !(Map ShipId BattleShip)
  , getSpaceMatrix :: ![[Material]] -- TODO ByteString would use 3 * 64 times less memory
  , getWorldId :: !(Maybe WorldId)
} deriving(Generic, Show)
instance Binary WorldEssence

newtype WorldId = WorldId Int64
  deriving(Generic, Show, Binary, Enum, Eq, NFData)

data World = World {
    getWorldNumbers :: ![Number]
    -- ^ The remaining 'Number's (shot 'Number's are removed from the list)
  , getWorldShips :: !(Map ShipId BattleShip)
  , getWorldSpace :: {-# UNPACK #-} !Space
    -- ^ The 'Space' in which 'BattleShip' and 'Number's evolve
  , getWorldRenderedSpace :: {-# UNPACK #-} !RenderedSpace
  , getParticleSystems :: !(Map ParticleSystemKey (Prioritized ParticleSystem))
    -- ^ Animated particle systems, illustrating player actions and important game events.
  , getId :: {-# UNPACK #-} !(Maybe WorldId)
} deriving (Generic)

newtype ParticleSystemKey = ParticleSystemKey Int
  deriving (Eq, Ord, Enum, Show, Num)

data ViewMode = CenterShip !ShipId
              -- ^ the 'BattleShip' position is fixed w.r.t the screen.
              | CenterSpace
              -- ^ the 'Space' frame is fixed w.r.t the screen.
              deriving(Show)

computeViewDistances :: ViewMode -> (Length Width, Length Height)
computeViewDistances (CenterShip _) = (30, 2) -- it will overlapp for large worlds but that's okay:
                                          -- the overlap is far away from where the ship is
computeViewDistances CenterSpace = (20, 2)

data BattleShip = BattleShip {
    getPilotName :: {-# UNPACK #-} !PlayerName
  , _shipPosSpeed :: !PosSpeed
  -- ^ Discrete position and speed.
  , getAmmo :: !Int
  -- ^ How many laser shots are left.
  , getShipStatus :: {-# UNPACK #-} !ShipStatus
  , getCollisions :: ![Number]
  -- ^ Which 'Number's are currently colliding with the 'BattleShip'.
} deriving(Generic, Show)
instance Binary BattleShip

data ShipStatus =
    Armored
  | Unarmored
  | Destroyed
  deriving(Generic, Show)
instance Binary ShipStatus

newtype ShipId = ShipId Int64
  deriving(Generic, Binary, Eq, Ord, Show, Enum, NFData)

data Number = Number {
    _numberPosSpeed :: !PosSpeed
  -- ^ Discrete position and speed.
  , getNumber :: {-# UNPACK #-} !Int
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
    _screenSize :: {-# UNPACK #-} !(Maybe Size)
  -- ^ Maybe we couldn't get the screen size.
  , _screenCenter :: {-# UNPACK #-} !(Coords Pos)
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

{-# INLINE findShip #-}
findShip :: ShipId -> (Map ShipId BattleShip) -> BattleShip
findShip i =
  fromMaybe (error $ "ship not found : " ++ show i)
  . lookup i
