{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-|
This is a multiplayer game where every player uses the keyboard as a synthesizer.

The music is shared between all players.
 -}

module Main where

import           Imj.Prelude

import           Control.Concurrent.MVar.Strict(MVar, modifyMVar, modifyMVar_, newMVar, readMVar)
import           Control.DeepSeq(NFData)
import           Control.Monad.State.Strict(gets, execStateT)
import           Control.Monad.Reader(asks)
import           Data.Binary(Binary(..))
import           Data.Map.Internal(Map(..))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import           Data.Text(pack, Text)
import           Data.Proxy(Proxy(..))
import           GHC.Generics(Generic)
import qualified Graphics.UI.GLFW as GLFW(Key(..), KeyState(..))

import           Imj.Categorized
import           Imj.ClientView.Types
import           Imj.Game.App(runGame)
import           Imj.Game.Class
import           Imj.Game.Command
import           Imj.Game.Modify
import           Imj.Game.Show
import           Imj.Game.Status
import           Imj.Geo.Discrete.Types
import           Imj.Graphics.Class.Positionable
import           Imj.Graphics.Color
import           Imj.Graphics.Screen
import           Imj.Graphics.Text.ColorString(ColorString)
import qualified Imj.Graphics.Text.ColorString as CS
import           Imj.Graphics.Text.Render
import           Imj.Graphics.UI.Colored
import           Imj.Graphics.UI.RectContainer
import           Imj.Music.Types
import           Imj.Music.Piano
import           Imj.Music.Record
import           Imj.Server.Class hiding(Do)
import           Imj.Server.Connection
import           Imj.Server.Types
import           Imj.Server
import           Imj.Timing

main :: IO ()
main = runGame (Proxy :: Proxy SynthsGame)

data LoopId = LoopId {
    _loopCreator :: {-# UNPACK #-} !ClientId
  , _loopIndex :: {-# UNPACK #-} !Int
} deriving(Generic, Show, Ord, Eq)
instance Binary LoopId
instance NFData LoopId

data SynthsGame = SynthsGame {
    _clientsPianos :: !(Map ClientId PianoState)
  , _clientLoopsPianos :: !(Map LoopId PianoState)
} deriving(Show)

data SynthsServer = SynthsServer
  deriving(Generic)

instance NFData SynthsServer

data SynthsClientView = SynthsClientView {
    _piano :: !PianoState
    -- ^ What is currently pressed, by the player (excluding what is played by loops)
  , _recording :: !Recording
    -- ^ The ongoing recording of what is being played, which can be used to create a 'Loop'
  , _serverLoopsPianos :: !(Map LoopId (MVar PianoState))
  -- ^ we don't need an 'MVar' for the 'Map' itself because when we read it or modify it, we
  -- are guaranteed not to have race condition, as the MVar of 'ServerState' is taken.
  , _nextLoopPianoIdx :: !Int
} deriving(Generic)
instance Show SynthsClientView where
  show (SynthsClientView p r s n) = show ("SynthsClientView" :: String,p,r,Map.keysSet s, n)
instance NFData SynthsClientView

thePianoValue :: SynthsClientView -> PianoState
thePianoValue (SynthsClientView x _ _ _) = x

-- | The server will also send 'PlayMusic' events, which are generic events.
data SynthsServerEvent =
  PianoValue !(Either ClientId LoopId) !PianoState
  deriving(Generic, Show)
instance DrawGroupMember SynthsServerEvent where
  exclusivityKeys = \case
    PianoValue {} -> mempty -- we do this is so that interleaving 'PianoValue' events with 'PlayMusic' events
                           -- will still allow multiple play music events to be part of the same frame.
                           -- It is a bit of a hack, the better solution would be to handle audio events
                           -- client-side as soon as they are received (in the listening thread) (TODO).

instance Categorized SynthsServerEvent
instance NFData SynthsServerEvent
instance Binary SynthsServerEvent

data SynthClientEvent =
    PlayNote !Music !Instrument
  | StartLoop
  | ForgetCurrentRecording
  deriving(Show,Generic)
instance Binary SynthClientEvent
instance Categorized SynthClientEvent

instance GameExternalUI SynthsGame where

  gameWindowTitle = const "Play some music!"

  -- NOTE 'getViewport' is never called unless 'putIGame' was called once.
  getViewport _ (Screen _ center) (SynthsGame _ _) =
    mkCenteredRectContainer center $ Size 40 100

data SynthsStatefullKeys
instance GameStatefullKeys SynthsGame SynthsStatefullKeys where

  mapStateKey _ (GLFW.Key'Space) GLFW.KeyState'Pressed _ _ =
    return $ Just $ CliEvt $ ClientAppEvt StartLoop
  mapStateKey _ (GLFW.Key'F10) GLFW.KeyState'Pressed _ _ =
    return $ Just $ CliEvt $ ClientAppEvt ForgetCurrentRecording
  mapStateKey _ k s _ _ =
    return $ CliEvt . ClientAppEvt . flip PlayNote SineSynth <$> n
   where
    n = maybe Nothing (\noteSpec -> case s of
      GLFW.KeyState'Pressed -> Just $ StartNote noteSpec 1
      GLFW.KeyState'Released -> Just $ StopNote noteSpec
      GLFW.KeyState'Repeating -> Nothing) $ keyToNote k
    keyToNote = \case
      -- NOTE GLFW uses the US keyboard layout to name keys: https://en.wikipedia.org/wiki/British_and_American_keyboards
      -- lower keys
      GLFW.Key'Z -> Just $ NoteSpec Do $ noOctave - 1
      GLFW.Key'S -> Just $ NoteSpec Réb $ noOctave - 1
      GLFW.Key'X -> Just $ NoteSpec Ré $ noOctave - 1
      GLFW.Key'D -> Just $ NoteSpec Mib $ noOctave - 1
      GLFW.Key'C -> Just $ NoteSpec Mi $ noOctave - 1
      GLFW.Key'V -> Just $ NoteSpec Fa $ noOctave - 1
      GLFW.Key'G -> Just $ NoteSpec Solb $ noOctave - 1
      GLFW.Key'B -> Just $ NoteSpec Sol $ noOctave - 1
      GLFW.Key'H -> Just $ NoteSpec Lab $ noOctave - 1
      GLFW.Key'N -> Just $ NoteSpec La $ noOctave - 1
      GLFW.Key'J -> Just $ NoteSpec Sib $ noOctave - 1
      GLFW.Key'M -> Just $ NoteSpec Si $ noOctave - 1
      GLFW.Key'Comma -> Just $ NoteSpec Do $ noOctave + 0
      GLFW.Key'L -> Just $ NoteSpec Réb $ noOctave + 0
      GLFW.Key'Period -> Just $ NoteSpec Ré $ noOctave + 0
      GLFW.Key'Semicolon -> Just $ NoteSpec Mib $ noOctave + 0
      GLFW.Key'Slash -> Just $ NoteSpec Mi $ noOctave + 0
      -- upper keys
      GLFW.Key'Q -> Just $ NoteSpec Do $ noOctave + 0
      GLFW.Key'2 -> Just $ NoteSpec Réb $ noOctave + 0
      GLFW.Key'W -> Just $ NoteSpec Ré $ noOctave + 0
      GLFW.Key'3 -> Just $ NoteSpec Mib $ noOctave + 0
      GLFW.Key'E -> Just $ NoteSpec Mi $ noOctave + 0
      GLFW.Key'R -> Just $ NoteSpec Fa $ noOctave + 0
      GLFW.Key'5 -> Just $ NoteSpec Solb $ noOctave + 0
      GLFW.Key'T -> Just $ NoteSpec Sol $ noOctave + 0
      GLFW.Key'6 -> Just $ NoteSpec Lab $ noOctave + 0
      GLFW.Key'Y -> Just $ NoteSpec La $ noOctave + 0
      GLFW.Key'7 -> Just $ NoteSpec Sib $ noOctave + 0
      GLFW.Key'U -> Just $ NoteSpec Si $ noOctave + 0
      GLFW.Key'I -> Just $ NoteSpec Do $ noOctave + 1
      GLFW.Key'9 -> Just $ NoteSpec Réb $ noOctave + 1
      GLFW.Key'O -> Just $ NoteSpec Ré $ noOctave + 1
      GLFW.Key'0 -> Just $ NoteSpec Mib $ noOctave + 1
      GLFW.Key'P -> Just $ NoteSpec Mi $ noOctave + 1
      GLFW.Key'LeftBracket -> Just $ NoteSpec Fa $ noOctave + 1
      GLFW.Key'Equal -> Just $ NoteSpec Solb $ noOctave + 1
      GLFW.Key'RightBracket -> Just $ NoteSpec Sol $ noOctave + 1
      _ -> Nothing

instance GameLogic SynthsGame where

  type ServerT SynthsGame = SynthsServer
  type StatefullKeysT SynthsGame = SynthsStatefullKeys

  mapInterpretedKey _ _ = return Nothing

  onClientOnlyEvent = \case
    () -> return ()
  onServerEvent = \case
    PianoValue creator x ->
      getIGame >>= \mayG -> do
        let (c,l) = maybe (mempty,mempty) (\(SynthsGame pianos loopPianos) -> (pianos,loopPianos)) mayG
            new = either
              (\i -> SynthsGame (Map.insert i x c) l)
              (\i -> SynthsGame c (Map.insert i x l))
              creator
        withAnim $ putIGame new -- TODO force withAnim when using putIGame ?

instance GameDraw SynthsGame where

  drawBackground (Screen _ center) (SynthsGame pianoClients pianoLoops) = do
    strs1 <- showPianos
              "Players"
              showPlayerName
              pianoClients
    strs2 <- showPianos
              "Loops"
              (\(LoopId creator idx) -> flip (<>) (" " <> CS.colored (pack (show idx)) (rgb 2 2 2)) <$> showPlayerName creator)
              pianoLoops
    let allStrs = strs1 ++ strs2
        maxL = fromMaybe 0 $ maximumMaybe $ map CS.countChars allStrs
        right = move (quot maxL 2) RIGHT center
    foldM_
      (\a str -> drawAligned str a)
      (mkRightAlign right)
      allStrs
    return center

instance ServerInit SynthsServer where

  type ClientViewT SynthsServer = SynthsClientView

  mkInitialState = return ((), SynthsServer)

  mkInitialClient = SynthsClientView mkEmptyPiano mkEmptyRecording mempty 0

instance ServerInParallel SynthsServer

instance ServerClientLifecycle SynthsServer where

  clientCanJoin _ = do
    -- A client has just connected, we make it be part of the current game:
    notifyClient' $ EnterState $ Included $ PlayLevel Running
    return True

instance ServerClientHandler SynthsServer where

  type StateValueT  SynthsServer = GameStateValue -- This is required

  type ClientEventT SynthsServer = SynthClientEvent

  handleClientEvent e = case e of
    PlayNote n i -> do
      onRecordableNote n i >>= notifyEveryoneN'
      return []
    ForgetCurrentRecording -> do
      adjustClient_ $ \s -> s {_recording = mkEmptyRecording }
      return []
    StartLoop -> do
      cid <- asks clientId
      fmap (_piano . unClientView) . Map.lookup cid <$> gets clientsMap >>= maybe
        (return [])
        (\piano -> do
          concat <$> forM (silencePiano piano) (flip onRecordableNote SineSynth) >>= notifyEveryoneN'
          fmap (_recording . unClientView) . Map.lookup cid <$> gets clientsMap >>= maybe
            (return [])
            (\recording -> mkLoop recording <$> liftIO getSystemTime >>= either
              (\msg -> do
                notifyClient' $ Warn msg
                return [])
              (\l -> do
                  pianoV <- liftIO $ newMVar mkEmptyPiano
                  creator <- asks clientId
                  (SynthsClientView _ _ _ idx) <- adjustClient
                    (\(SynthsClientView _ _ loops loopIdx) ->
                      SynthsClientView
                        mkEmptyPiano
                        mkEmptyRecording
                        (Map.insert (LoopId creator loopIdx) pianoV loops)
                        $ loopIdx + 1)
                  return
                    [ (\v -> forever $ playLoopOnce -- TODO make loops cancelable using an IORef.
                        (\a -> modifyMVar_ v . execStateT . playLoopMusic (LoopId creator $ idx-1) pianoV a)
                        l)
                    ])))
   where

    onRecordableNote :: (MonadIO m, MonadState (ServerState SynthsServer) m, MonadReader ConstClientView m)
                       => Music
                       -> Instrument
                       -> m [ServerEvent SynthsServer]
    onRecordableNote n i = do
      cid <- asks clientId
      fmap (_piano . unClientView) . Map.lookup cid <$> gets clientsMap >>= maybe
        (return []) -- should never happen
        (\piano -> do
          let (countChangedNotes, piano') = modPiano n piano
          if countChangedNotes == 0
            then
              return []
            else do
              t <- liftIO getSystemTime
              creator <- asks clientId
              newpiano <- _piano <$> adjustClient (\s@(SynthsClientView _ recording _ _) ->
                    s { _piano = piano'
                      , _recording = recordMusic t recording n i })
              return
                [ ServerAppEvt $ PianoValue (Left creator) newpiano
                , PlayMusic n i
                ])

    playLoopMusic :: (MonadState (ServerState SynthsServer) m, MonadIO m)
                  => LoopId -> MVar PianoState -> Music -> Instrument -> m ()
    playLoopMusic loopId p n i = do
      (countChangedNotes,newPiano) <- liftIO $ modifyMVar p $ \prevPiano -> do
        let (c,newPiano) = modPiano n prevPiano
        return (newPiano,(c, newPiano))
      notifyEveryoneN' $ if countChangedNotes == 0
        then
          []
        else
          [ ServerAppEvt $ PianoValue (Right loopId) newPiano
          , PlayMusic n i
          ]
instance Server SynthsServer where

  type ServerEventT SynthsServer = SynthsServerEvent

  greetNewcomer' = -- Send the currently started notes so that the newcomer
                   -- hears exactly what other players are hearing.
    map (fmap unClientView) . Map.assocs <$> gets clientsMap >>= \l -> do
      concat <$> mapM
        (\(i,(SynthsClientView piano _ m _)) -> do
          concat . (:) (pianoEvts (Left i) piano) <$>
            mapM
              (\(loopIdx,p) -> pianoEvts (Right loopIdx) <$> liftIO (readMVar p))
              (Map.assocs m))
        l

pianoEvts :: Either ClientId LoopId -> PianoState -> [ServerEvent SynthsServer]
pianoEvts idx v@(PianoState m) =
  ServerAppEvt (PianoValue idx v) :
  concatMap
    (\(note,n) ->
      replicate n $ PlayMusic (StartNote note 1) SineSynth)
    (Map.assocs m)

instance ServerCmdParser SynthsServer


{-# INLINABLE showPianos #-}
showPianos :: MonadReader e m
           => Text
           -> (a -> m ColorString)
           -> Map a PianoState
           -> m [ColorString]
showPianos _ _ Tip = return []
showPianos title showKey m = do
  let minNote = NoteSpec Do  $ noOctave - 1
      maxNote = NoteSpec Sol $ noOctave + 1
  showArray (Just (CS.colored title $ rgb 2 1 2,"")) <$> mapM
    (\(i,piano) -> flip (,) (CS.colored (pack $ showKeys minNote maxNote piano) $ rgb 3 1 2) <$> showKey i)
    (Map.assocs m)

showKeys :: NoteSpec
         -- ^ From
         -> NoteSpec
         -- ^ To
         -> PianoState
         -> String
showKeys from to (PianoState m) =
  go [] [from..to] $ Set.toAscList $ Map.keysSet m
 where
  go l [] _ = reverse l
  go l (k@(NoteSpec n _):ks) remainingPressed =
    case remainingPressed of
      [] -> go' freeChar remainingPressed
      p:ps ->
        if p == k
          then
            go' pressedChar ps
          else
            go' freeChar remainingPressed
   where
    go' c remPressed = go (maybeToList space ++ [c] ++ l) ks remPressed

    space = case ks of
      [] -> Nothing
      (NoteSpec s _):_ -> case s of
        Fa -> Just ' '
        Do -> Just ' '
        _ -> Nothing

    freeChar
      | naturalNote n = '-'
      | otherwise = '*'--'\''

    pressedChar
      | naturalNote n = '_'
      | otherwise = '.'
