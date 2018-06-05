{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Imj.Game.Class
      (
      -- * Classes
        Client(..)
      , GameLogic(..)
      , GameExternalUI(..)
      , GameDraw(..)
      , GameStatefullKeys(..)
      , DrawGroupMember(..)
      -- * Client / GameLogic
      , EventsForClient(..)
      , Game(..)
      , AnimatedLine(..)
      , GenEvent(..)
      , UpdateEvent
      , CustomUpdateEvent
      , EventGroup(..)
      , DrawGroupKeys(..)
      , defaultFrameSize
      -- * AppState type
      , AppState(..)
      , GameState(..)
      , RecordMode(..)
      , OccurencesHist(..)
      , Occurences(..)
      , EventCategory(..)
      -- * Player
      , Player(..)
      , mkPlayer
      , PlayerColors(..)
      , mkPlayerColors
      , ColorTheme(..)
      -- * Helper types
      , Transitioning(..)
      , GameArgs(..)
      , Infos(..)
      , mkEmptyInfos
      -- * EventGroup
      , mkEmptyGroup
      , visible
      , count
      , tryGrow
      -- * reexports
      , MonadState
      , TQueue
      ) where

import           Imj.Prelude hiding(range)
import           Prelude(length)

import           Control.Concurrent.STM(TQueue)
import           Control.Monad.State.Class(MonadState)
import           Control.Monad.Reader.Class(MonadReader)
import           Data.Set(Set)
import qualified Data.Set as Set
import           Data.Map.Strict(Map)
import           Data.Proxy(Proxy(..))
import qualified Graphics.UI.GLFW as GLFW(Key(..), KeyState(..), ModifierKeys(..))

import           Imj.Arg.Class
import           Imj.Categorized
import           Imj.ClientView.Types
import           Imj.Control.Concurrent.AsyncGroups.Class
import           Imj.Event
import           Imj.Game.Audio.Class
import           Imj.Game.Configuration
import           Imj.Game.ColorTheme.Class
import           Imj.Game.Infos
import           Imj.Game.Priorities
import           Imj.Game.Status
import           Imj.Geo.Discrete.Types
import           Imj.Graphics.Class.DiscreteDistance
import           Imj.Graphics.Class.Draw
import           Imj.Graphics.Class.HasSizedFace
import           Imj.Graphics.Class.Render
import           Imj.Graphics.Color
import           Imj.Graphics.Font
import           Imj.Graphics.Interpolation.Evolution
import           Imj.Graphics.ParticleSystem
import           Imj.Graphics.Render.Delta.Backend.OpenGL(PreferredScreenSize(..))
import           Imj.Graphics.RecordDraw
import           Imj.Graphics.Screen
import           Imj.Graphics.UI.Animation
import           Imj.Graphics.UI.RectContainer
import           Imj.Input.Types
import           Imj.Network
import           Imj.Server.Class
import           Imj.Server.Color
import           Imj.Server.Types
import           Imj.ServerView.Types

import           Imj.Graphics.UI.Chat
import           Imj.Game.Timing
import           Imj.Graphics.Text.ColoredGlyphList
import           Imj.Graphics.Text.ColorString

class GameLogic (GameLogicT c)
  => Client c
 where
  type GameLogicT c

  -- | Send a 'ClientEvent' to the server.
  sendToServer' :: (MonadIO m)
                => c
                -> ClientEvent (ServerT (GameLogicT c))
                -> m ()

  -- | The queue containing events that should be handled by the client.
  serverQueue :: c -> TQueue (EventsForClient (GameLogicT c))

  -- | Fill 'serverQueue'
  writeToClient' :: (MonadIO m)
                 => c -> EventsForClient (GameLogicT c) -> m ()

data EventsForClient g =
    FromClient !(Event (ClientOnlyEvtT g))
  | FromServer !(ServerEvent (ServerT g))
  deriving(Generic)
instance (GameLogic g) => Show (EventsForClient g) where
  show (FromClient e) = show ("FromClient", e)
  show (FromServer e) = show ("FromServer", e)

data GenEvent g =
    Evt {-unpack sum-} !(Event (ClientOnlyEvtT g))
    -- ^ Generated by the client, handled by the client immediately after creation.
  | CliEvt {-unpack sum-} !(ClientEvent (ServerT g))
    -- ^ Generated by the client, sent to the 'ServerT', which in turn may send back some 'ServerEvent'.
  | SrvEvt {-unpack sum-} !(ServerEvent (ServerT g))
    -- ^ Generated by either 'ServerT' or the client, handled by the client immediately upon reception.
    deriving(Generic)
instance GameLogic g => Show (GenEvent g) where
  show (Evt e) = show("Evt",e)
  show (CliEvt e) = show("CliEvt",e)
  show (SrvEvt e) = show("SrvEvt",e)

-- | Specifies which world we want information about (the one that is already displayed
-- is 'From', the new one is 'To'.)
data Transitioning = From | To

-- | 'GameExternalUI' defines informations used by 'withAnim', /before/ and then /after/
-- applying the action, to compute the resulting external UI animations.
class (LeftInfo (ClientInfoT g))
      => GameExternalUI g where
  type ClientInfoT g
  type ClientInfoT g = ()

  -- | This name is used to set the title of the game window.
  gameWindowTitle :: Proxy g -> String

  -- | Returns the color of the external frame. Defaults to 'rgb 2 1 1'.
  getFrameColor :: Maybe g
                -> LayeredColor
  getFrameColor _ = onBlack $ rgb 2 1 1

  -- | Returns the position of the external frame.
  getViewport :: Transitioning
              -> Screen
              -> g
              -> RectContainer

  -- | See 'ClientInfoT'. It is used to compute the player informations
  -- on the left of the external frame.
  --
  -- Defaults to an empty 'Map'.
  getClientsInfos :: Transitioning
                  -> g
                  -> Map ClientId (ClientInfoT g)
  getClientsInfos _ _ = mempty

  -- | It is used to compute the player informations on the left of the external frame.
  mkWorldInfos :: InfoType
               -> Transitioning
               -> g
               -> Infos
  mkWorldInfos _ _ _ = mkEmptyInfos

-- | Statefull keys are supported only by the GLFW backend.
-- We can't distinguish key press / key release when using the terminal backend.
--
-- Hence, if you need statefull key support (i.e you have some code in 'mapStateKey'),
-- 'needsStatefullKeys' should return 'True', so as to forbid the usage of the terminal backend
-- (this will remove the command line option for chosing the render backend,
-- and the backend value will be forced to GLFW).
class GameStatefullKeys g s where

  -- | When this returns 'True', the game can be played exclusively on the GLFW window.
  -- The default implementation returns 'True'
  needsStatefullKeys :: Proxy s -> Proxy g -> Bool
  needsStatefullKeys _ = const True
  {-# INLINE needsStatefullKeys #-}

  -- | Maps a 'GLFW.Key' to a list of 'GenEvent', given a 'GameStateValue'.
  --
  -- This method is called only when the client 'StateNature' is 'Ongoing', and
  -- when the 'StateValue' is 'Included' @_@.
  --
  -- Note that 'Shift + Arrow' key-presses are dedicated to font selection, and will be filtered
  -- before reaching this function.
  mapStateKey :: (GameLogic g
                , GameLogicT e ~ g
                , s ~ StatefullKeysT g
                , MonadReader e m, Client e)
              => Proxy s
              -> GLFW.Key
              -> GLFW.KeyState
              -> GLFW.ModifierKeys
              -> GameStateValue
              -- ^ The current client state.
              -> Game g
              -> m [GenEvent g]

instance GameStatefullKeys g () where
  needsStatefullKeys _ = const False
  mapStateKey _ _ _ _ _ _ = return []

-- | 'GameLogic' Formalizes the client-side logic of a multiplayer game.
class (Show g
     , GameExternalUI g, GameDraw g
     , Server (ServerT g), ServerClientHandler (ServerT g)
     , Audio (AudioT g), Arg (AudioT g), Show (AudioT g)
     , GameStatefullKeys g (StatefullKeysT g)
     , Categorized (ClientOnlyEvtT g)
     , Show (ClientOnlyEvtT g)
     , ColorTheme (ColorThemeT g)
     , Binary (ColorThemeT g)
     )
      =>
     GameLogic g
     where

  -- | Server-side dual of 'GameLogic'
  type ServerT g = (r :: *) | r -> g

  -- | Audio backend
  type AudioT g = (r :: *) | r -> g
  type AudioT g = WithAudio -- enable audio by default (use '()' to disable it)

  -- | Events generated on the client and handled by the client.
  type ClientOnlyEvtT g
  type ClientOnlyEvtT g = ()

  -- | The colors used by a player
  type ColorThemeT g
  type ColorThemeT g = ()

  -- | Statefull key handling (i.e key press / key release / key repeat, modifiers).
  type StatefullKeysT g
  type StatefullKeysT g = () -- The () instance doesn't support stateful keys.

  onAnimFinished :: (GameLogicT e ~ g
                   , MonadState (AppState (GameLogicT e)) m
                   , MonadReader e m, Client e
                   , MonadIO m)
                 => m ()
  onAnimFinished = return ()

  onServerEvent :: (g ~ GameLogicT e
                  , MonadState (AppState g) m
                  , MonadReader e m, Client e, Render e, HasSizedFace e, AsyncGroups e, Audio e
                  , MonadIO m)
                => ServerEventT (ServerT g)
                -> m ()
  onClientOnlyEvent :: (g ~ GameLogicT e
                      , MonadState (AppState g) m
                      , MonadReader e m, Client e, Render e, HasSizedFace e, AsyncGroups e, Audio e
                      , MonadIO m)
                    => ClientOnlyEvtT g
                    -> m ()

  onClientCustomCmd :: (MonadState (AppState g) m
                      , MonadIO m)
                    => CustomCmdT (ServerT g)
                    -> m ()
  onClientCustomCmd = fail "you should implement this if you have custom commands."

  -- | Maps a 'Key' to a list of 'GenEvent', given a 'GameStateValue'.
  --
  -- This method is called only when the client 'StateNature' is 'Ongoing', and
  -- when the 'StateValue' is 'Included' @_@.
  mapInterpretedKey :: (GameLogicT e ~ g
                     , MonadReader e m, Client e)
                    => Key
                    -> GameStateValue
                    -- ^ The current client state.
                    -> Game g
                    -> m [GenEvent g]

-- | At every render, first background elements ('drawBackground') are drawn,
-- then particle systems (see 'addParticleSystems' to add particle systems),
-- and finally the foreground elements are drawn ('drawForeground')
class GameDraw g where

  -- | Draw the background layer (i.e /before/ particle system animations are drawn)
  -- and returns a reference position that will be used to position particle systems
  -- animations, and will be passed to 'drawForeground'.
  drawBackground :: (GameLogicT e ~ g
                   , MonadState (AppState (GameLogicT e)) m
                   , MonadIO m, MonadReader e m, Draw e)
                 => Screen
                 -> g
                 -> m (Coords Pos)
                 -- ^ The reference position for particle systems.
  drawBackground (Screen _ center) _ = return center

  -- | Draw the foreground layer (i.e /after/ particle system animations are drawn)
  drawForeground :: (GameLogicT e ~ g
                   , MonadState (AppState (GameLogicT e)) m
                   , MonadIO m, MonadReader e m, Draw e)
                 => Screen
                 -> Coords Pos
                 -- ^ The reference position for particle systems as returned by 'drawBackground'.
                 -> g
                 -> m ()
  drawForeground _ _ _ = return ()

defaultFrameSize :: Size
defaultFrameSize = Size 20 20

data Infos = Infos {
    upInfos, downInfos :: !(Successive ColoredGlyphList)
  , leftUpInfos :: [Successive ColoredGlyphList]
  , leftDownInfos :: [Successive ColoredGlyphList]
}

mkEmptyInfos :: Infos
mkEmptyInfos = Infos (Successive [fromString ""]) (Successive [fromString ""]) [] []

data EventGroup g = EventGroup {
    events :: ![UpdateEvent g]
  , _eventGroupKeys :: !(Set DrawGroupKeys)
  , evtGroupUpdateDuration :: !(Time Duration System)
  , _eventGroupVisibleTimeRange :: !(Maybe (TimeRange System))
  -- ^ TimeRange of /visible/ events deadlines
}

-- | Regroups events that can be handled immediately by the client.
type UpdateEvent g  = Either (ServerEvent (ServerT g)) (Event (ClientOnlyEvtT g))
type CustomUpdateEvent g = Either (ServerEventT (ServerT g)) (ClientOnlyEvtT g)

data DrawGroupKeys =
    RedrawStatusKey
  | GameStep
  deriving(Eq, Ord)

class DrawGroupMember e where
  -- | Any two events of the same 'EventGroup' must have non-overlapping 'exclusivityKeys'.
  --
  -- Returning an empty 'Set' ('mempty') will allow the event to be member of any 'EventGroup',
  -- regardless of events already present in it, and it will not prevent any further event to be included
  -- in the same 'EventGroup'.
  --
  -- WARNING : If the render backend of your game has a minimal duration @minDt@ between two rendered frames
  -- (which is the case with single buffered opengl rendering) and if an event for which this function
  -- returns a non-empty set is generated continuously at a frequency bigger than the inverse of @minDt@::
  --
  -- * player input will be ignored (TODO We could "parallelize" platform events consumption
  -- so that player input can still be handled correctly in that case and still keep the guarantee that
  -- server events are handled in their order of arrival.)
  -- * only a single of these event can be present per rendered frame, so the queue of server events
  -- will grow bigger and bigger, and these events will ultimately overflow the queue / delay other events.
  -- Also, in a multi-player setting, no 2 players will see the same behaviour,
  -- unless their respective @minDt@ are strictly equal, which is unlikely.
  --
  -- Hence, if an event is generated at high frequency, the recommendation is to
  -- return 'mempty', else your game / application may exhibit strong lag and/or unresponsiveness.
  exclusivityKeys :: e -> Set DrawGroupKeys

instance DrawGroupMember () where
  exclusivityKeys () = mempty

instance (DrawGroupMember e, DrawGroupMember f)
  => DrawGroupMember (Either e f) where
  exclusivityKeys = either exclusivityKeys exclusivityKeys

instance DrawGroupMember e
  => DrawGroupMember (Event e) where
  exclusivityKeys = \case
    (Timeout (Deadline _ _ (AnimateParticleSystem _))) -> mempty
    (Timeout (Deadline _ _ AnimateUI)) -> mempty
    (Timeout (Deadline _ _ RedrawStatus{})) -> Set.singleton RedrawStatusKey
    Log {} -> mempty
    ToggleEventRecording -> mempty
    CanvasSizeChanged -> mempty
    RenderingTargetChanged  -> mempty
    CycleRenderingOptions {}  -> mempty
    ApplyPPUDelta {} -> mempty
    ApplyFontMarginDelta {} -> mempty
    ChatCmd {} -> mempty
    SendChatMessage -> mempty
    AppEvent e -> exclusivityKeys e

instance DrawGroupMember (ServerEventT e)
  => DrawGroupMember (ServerEvent e) where
  exclusivityKeys = \case
    ServerAppEvt x -> exclusivityKeys x
    PlayMusic {} -> mempty
    CommandError {} -> mempty
    RunCommand {} -> mempty
    Reporting {} -> mempty
    PlayerInfo {} -> mempty
    ConnectionAccepted {} -> mempty
    ConnectionRefused {} -> mempty
    Disconnected {} -> mempty
    OnContent {} -> mempty
    AllClients {} -> mempty
    EnterState {} -> mempty
    ExitState {} -> mempty
    ServerError {} -> mempty
    Warn {} -> mempty

mkEmptyGroup :: EventGroup g
mkEmptyGroup = EventGroup [] mempty zeroDuration Nothing

visible :: EventGroup g -> Bool
visible (EventGroup _ _ _ Nothing) = False
visible _ = True

count :: EventGroup g -> Int
count (EventGroup l _ _ _) = length l

tryGrow :: (DrawGroupMember (ServerEventT (ServerT g))
          , DrawGroupMember (ClientOnlyEvtT g))
        => Maybe (UpdateEvent g) -> EventGroup g -> IO (Maybe (EventGroup g))
tryGrow Nothing group
 | null $ events group = return $ Just group -- Keep the group opened to NOT do a render
 | otherwise = return Nothing -- to do a render
tryGrow (Just e) (EventGroup l pastKeys updateTime range)
 | keyConflict = return Nothing
 | updateTime > fromSecs 0.01 = return Nothing -- we limit the duration of updates, to keep a stable render rate
 | otherwise = maybe mkRangeSingleton (flip extendRange) range <$> time >>= \range' -> return $
    let maxDiameter = particleSystemDurationToSystemDuration $ 0.99 .* particleSystemPeriod
    in if timeSpan range' > maxDiameter
      then
        Nothing -- trigger a render so that no 2 updates of the same particle system are done in the same group
      else
        withEvent $ Just range'
 where
  thisKeys = exclusivityKeys e
  mergedKeys = Set.union pastKeys thisKeys
  keyConflict = Set.size thisKeys + Set.size pastKeys /= Set.size mergedKeys -- True when pastKeys and thisKeys overlapp
  withEvent = Just . EventGroup (e:l) mergedKeys updateTime
  time = case e of
    Right (Timeout (Deadline t _ _)) -> return t
    _ -> getSystemTime

type ParticleSystems = Map ParticleSystemKey (Prioritized ParticleSystem)

data Game g = Game {
    getClientState :: {-# UNPACK #-} !(ClientState GameStateValue)
  , getScreen :: {-# UNPACK #-} !Screen
  , getGameState' :: !(GameState g)
  , gameParticleSystems :: !ParticleSystems
    -- ^ Inter-level animation.
  , getDrawnClientState :: ![(ColorString    -- 'ColorString' is used to compare with new messages.
                             ,AnimatedLine)] -- 'AnimatedLine' is used for rendering.
  , getPlayers' :: !(Map ClientId (Player g))
  , _gameSuggestedClientName :: !(Maybe (ConnectIdT (ServerT g)))
  , getServerView' :: {-unpack sum-} !(ServerView (ValuesT (ServerT g)))
  -- ^ The server that runs the game
  , connection' :: (Maybe (Either Text ClientId))
  , getChat' :: !Chat
}

data GameState g = GameState {
    _game :: !(Maybe g)
  , _anim :: !UIAnimation
}

data Player g = Player {
    getPlayerName :: {-# UNPACK #-} !(ClientName Approved)
  , getClientStatus :: {-unpack sum-} !ClientStatus
  , getPlayerColors :: {-# UNPACK #-} !(PlayerColors g)
} deriving(Generic, Show)
instance GameLogic g => Binary (Player g)

mkPlayer :: GameLogic g => ClientEssence -> Player g
mkPlayer (ClientEssence a b color) =
  Player a b $ mkPlayerColors color

mkPlayerColors :: GameLogic g
               => Color8 Foreground
               -> PlayerColors g
mkPlayerColors c = PlayerColors c $ mkColorTheme c

data PlayerColors g = PlayerColors {
    getPlayerColor :: {-# UNPACK #-} !(Color8 Foreground)
    -- ^ Main color of player
  , getColorCycles :: !(ColorThemeT g)
} deriving(Generic)
instance GameLogic g => Binary (PlayerColors g)
instance GameLogic g => Show (PlayerColors g) where
  show (PlayerColors c cy) = show ("PlayerColors",c,cy)
instance GameLogic g => Eq (PlayerColors g) where
  (PlayerColors c _) == (PlayerColors c' _) = c == c'

data AnimatedLine = AnimatedLine {
    getRecordDrawEvolution :: !(Evolution RecordDraw)
  , getALFrame :: !Frame
  , getALDeadline :: !(Maybe Deadline)
} deriving(Generic, Show)

data Occurences a = Occurences {
    _occurencesCount :: {-# UNPACK #-} !Int
  , _occurencesItem :: {-unpack sum-} !EventCategory
} deriving(Generic, Show)

data AppState g = AppState {
    timeAfterRender :: !(Time Point System)
  , game :: !(Game g)
  , eventsGroup :: !(EventGroup g)
  , eventHistory :: !OccurencesHist
  -- ^ Can record which events where handled, for debugging purposes.
  , appStateRecordEvents :: !RecordMode
  -- ^ Should the handled events be recorded?
  , nextParticleSystemKey :: !ParticleSystemKey
  , appStateDebug :: {-unpack sum-} !Debug
  -- ^ Print times and group information in the terminal.
  , pressedKeys :: !(Set GLFW.Key)
}

data RecordMode = Record
                | DontRecord
                deriving(Eq)

data OccurencesHist = OccurencesHist {
    _occurencesHistList :: ![Occurences EventCategory]
  , _occurencesHistTailStr :: !ColorString
} deriving(Generic, Show)


data GameArgs g = GameArgs
  !ServerOnly
  !(Maybe ServerName)
  !(Maybe ArgServerPort)
  !(Maybe ServerLogs)
  !(Maybe ColorScheme)
  !(Maybe (ServerArgsT (ServerT g)))
  !(Maybe (ConnectIdT (ServerT g)))
  !(Maybe BackendType)
  !(Maybe PPU)
  !(Maybe PreferredScreenSize)
  !Debug
  !(Maybe (AudioT g))
