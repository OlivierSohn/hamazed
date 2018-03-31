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
        , WorldShape(..)
        , BattleShip(..)
        , ShipId(..)
        , ShipStatus(..)
        , shipIsAlive
        , Number(..)
        , NumberEssence(..)
        , NumberType(..)
        , NumId(..)
        , mkNumber
        , makeUnreachable
        , makeReachable
        , makePreferred
        , makeDangerous
        , getCurrentColor
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
        , Map
        , Set
        ) where

import           Imj.Prelude

import qualified System.Console.Terminal.Size as Terminal(Window(..))
import           Control.DeepSeq(NFData)
import           Data.Map.Strict(Map)
import qualified Data.Map.Strict as Map(lookup, filter)
import           Data.Set(Set)

import           Imj.Game.Hamazed.World.Space.Types
import           Imj.Game.Hamazed.Level.Types
import           Imj.Geo.Continuous.Types
import           Imj.Graphics.Color.Types
import           Imj.Graphics.ParticleSystem.Design.Types
import           Imj.Physics.Discrete.Types

import           Imj.Game.Hamazed.Loop.Event.Priorities
import           Imj.Game.Hamazed.Color
import           Imj.Graphics.Text.Animation
import           Imj.Graphics.UI.RectArea
import           Imj.Graphics.UI.RectContainer
import           Imj.Iteration
import           Imj.Physics.Discrete
import           Imj.Timing

data WorldParameters = WorldParameters {
    worldShape :: !WorldShape
  , wallDistrib :: !WallDistribution
} deriving(Generic, Show, Eq)

instance Binary WorldParameters
instance NFData WorldParameters

data WorldShape = Square
                -- ^ Width = Height
                | Rectangle'2x1
                -- ^ Width = 2 * Height
                deriving(Generic, Show, Eq)

instance Binary WorldShape
instance NFData WorldShape

data WorldSpec = WorldSpec {
    getLevelSpec' :: {-unpack sum-} !LevelSpec
  , getShipIds :: !(Set ShipId)
  , getWorldParams :: {-# UNPACK #-} !WorldParameters
} deriving(Generic, Show)
instance Binary WorldSpec
instance NFData WorldSpec

-- | Contains the minimal information needed to describe all parameters of the 'World'
-- that matter to the game (i.e we ignore particle system animations and objects used to optimize rendering)
data WorldEssence = WorldEssence {
    getNumbers :: !(Map NumId NumberEssence)
  , getShips :: !(Map ShipId BattleShip) -- TODO remove ShipId from BattleShip
  , getSpace :: !Space
} deriving(Generic, Show)
instance Binary WorldEssence

newtype WorldId = WorldId Int64
  deriving(Generic, Show, Binary, Enum, Eq, Ord, NFData)

data World = World {
    getWorldNumbers :: !(Map NumId Number)
    -- ^ The remaining 'Number's (shot 'Number's are removed from the list)
  , getWorldShips :: !(Map ShipId BattleShip)
  , getWorldSpace :: {-# UNPACK #-} !Space
    -- ^ The 'Space' in which 'BattleShip' and 'Number's evolve
  , getWorldRenderedSpace :: !RenderedSpace
  , getParticleSystems :: !(Map ParticleSystemKey (Prioritized ParticleSystem))
    -- ^ Animated particle systems, illustrating player actions and important game events.
  , getId :: {-unpack sum-} !(Maybe WorldId)
} deriving (Generic)

newtype NumId = NumId Int
  deriving (Generic, Ord, Eq, Binary, Show)

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
    getPilotId :: {-# UNPACK #-} !ShipId
  , shipPosSpeed :: !PosSpeed
  -- ^ Discrete position and speed.
  , getAmmo :: !Int
  -- ^ How many laser shots are left.
  , getShipStatus :: {-unpack sum-} !ShipStatus
  , getCollisions :: !(Set NumId)
  -- ^ Which 'Number's are currently colliding with the 'BattleShip'.
  , getShipCC:: {-# UNPACK #-} !ComponentIdx
  -- ^ The component in which the ship is located.
} deriving(Generic, Show)
instance Binary BattleShip

data ShipStatus =
    Armored
  | Unarmored
  | Destroyed
  deriving(Generic, Show)
instance Binary ShipStatus

{-# INLINE shipIsAlive #-}
shipIsAlive :: ShipStatus -> Bool
shipIsAlive Destroyed = False
shipIsAlive Unarmored = True
shipIsAlive Armored = True

newtype ShipId = ShipId Int64
  deriving(Generic, Binary, Eq, Ord, Show, Enum, NFData)

data NumberEssence = NumberEssence {
    getNumPosSpeed :: !PosSpeed
  -- ^ Discrete position and speed.
  , getNumber :: {-# UNPACK #-} !Int
  -- ^ Which number it represents (1 to 16).
  , getNumberCC :: {-# UNPACK #-} !ComponentIdx
  -- ^ The component in which the number is located.
} deriving(Generic, Show)
instance Binary NumberEssence

data Number = Number {
    getNumEssence :: !NumberEssence
  -- ^ The component in which the number is located.
  , getNumColor :: [Color8 Foreground]
  -- ^ Defines a sequence of colors, in time, or a single color.
  , getNumType :: !NumberType
} deriving(Generic, Show)

mkNumber :: NumberEssence -> Number
mkNumber e = Number e [] Reachable

data NumberType =
    Reachable
  | Unreachable
  | Preferred
  | Dangerous
  deriving(Generic, Show)

getCurrentColor :: Number -> Color8 Foreground
getCurrentColor (Number _ (color:_) _) = color
getCurrentColor (Number (NumberEssence _ i _) [] Reachable) = numberColor i
getCurrentColor (Number (NumberEssence _ i _) [] Unreachable) = unreachableNumberColor i
getCurrentColor (Number (NumberEssence _ i _) [] Dangerous) = dangerousNumberColor i
getCurrentColor (Number (NumberEssence _ i _) [] Preferred) = preferredNumberColor i

makeUnreachable :: Number -> Number
makeUnreachable n@(Number _ _ Unreachable) = n
makeUnreachable n@(Number e@(NumberEssence _ i _) _ _) = Number e newColor Unreachable
 where
  c' = unreachableNumberColor i
  startColor = getCurrentColor n
  newColor = take (bresenhamColor8Length startColor c') $ bresenhamColor8 startColor c'

makeDangerous :: Number -> Number
makeDangerous n@(Number _ _ Unreachable) = n
makeDangerous n@(Number e@(NumberEssence _ i _) _ _) = Number e newColor Dangerous
 where
  c' = dangerousNumberColor i
  startColor = getCurrentColor n
  newColor = take (bresenhamColor8Length startColor c') $ bresenhamColor8 startColor c'

makePreferred :: Number -> Number
makePreferred n@(Number e@(NumberEssence _ i _) _ Reachable) = Number e newColor Preferred
 where
  c' = preferredNumberColor i
  startColor = getCurrentColor n
  newColor = take (bresenhamColor8Length startColor c') $ bresenhamColor8 startColor c'
makePreferred n = n

makeReachable :: Number -> Number
makeReachable n@(Number e@(NumberEssence _ i _) _ Preferred) = Number e newColor Reachable
 where
  c' = numberColor i
  startColor = getCurrentColor n
  newColor = take (bresenhamColor8Length startColor c') $ bresenhamColor8 startColor c'
makeReachable n = n

getColliding :: Coords Pos -> Map NumId Number -> Map NumId Number
getColliding pos =
  Map.filter ((pos ==) . getPos . getNumPosSpeed . getNumEssence)

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
      in diffCoords worldCenter $ getPos $ shipPosSpeed $ findShip myId ships

getWorldCorner :: World -> Coords Pos -> Coords Pos -> Coords Pos
getWorldCorner world screenCenter offset =
  let (h',w') = (quot h 2, quot w 2)
      (Size h w) = getSize $ getWorldSpace world
  in sumCoords offset $ sumCoords screenCenter $ toCoords (-h') (-w')


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
    WorldScope mat ->
      if worldArea `contains` pos && mat == unsafeGetMaterial pos space
        then
          InsideWorld
        else
          OutsideWorld
    NegativeWorldContainer ->
      if worldViewArea `contains` pos
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
    _screenSize :: {-unpack sum-} !(Maybe Size)
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
findShip :: ShipId -> Map ShipId BattleShip -> BattleShip
findShip i =
  fromMaybe (error $ "ship not found : " ++ show i)
  . Map.lookup i
