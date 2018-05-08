{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Imj.Server.Run
      ( Server(..)
      , ServerState(..)
      , ServerEvent(..)
      , ClientId(..)
      , ClientEvent(..)
      , ServerOwnership(..)
      , ServerLogs(..)
      , DisconnectReason(..)
      , appSrv
      , shutdown
      , handlerError
      , error'
      , checkNameAvailability
      , checkName
      , publish
      ) where

import           Imj.Prelude

import           Control.Concurrent(threadDelay, forkIO)
import           Control.Concurrent.MVar.Strict (MVar)
import           Control.Monad.IO.Unlift(MonadIO, MonadUnliftIO)
import           Control.Monad.Reader(runReaderT, asks)
import           Control.Monad.State.Strict(StateT, runStateT, execStateT, modify', state, gets)
import qualified Data.List as List(intercalate)
import qualified Data.Map.Strict as Map
import           Data.Map.Strict((!?))
import           Data.Proxy(Proxy(..))
import qualified Data.Set as Set
import           Data.Text(pack)
import           Data.Tuple(swap)
import           Network.WebSockets(PendingConnection, Connection, acceptRequest, receiveData, sendBinaryData, sendPing)
import           UnliftIO.Exception (SomeException(..), try)
import           UnliftIO.MVar (modifyMVar_, modifyMVar)

import           Imj.ClientView.Internal.Types
import           Imj.Graphics.Color.Types
import           Imj.Server.Internal.Types
import           Imj.Server.Class
import           Imj.Server.Color
import           Imj.Server.Types

import           Imj.Graphics.Text.ColorString(colored)
import           Imj.Network
import           Imj.Server.Connection
import           Imj.Server
import           Imj.Server.Log
import           Imj.Timing

appSrv :: (Server s) => MVar (ServerState s) -> PendingConnection -> IO ()
appSrv st pending =
  acceptRequest pending >>= application st

-- | Handles the connection to one client. Synchronous exceptions
-- (network failures or players disconnections) are handled so that
-- a broken connection of /another/ client while broadcasting a message
-- won't impact the handled client.
application :: Server s => MVar (ServerState s) -> Connection -> IO ()
application st conn =
  receiveData conn >>= \case
    Connect macs mayConnectId cliType ->
      either
        refuse
        (\_ ->
          modifyMVar st (fmap swap . runStateT associateClientId) >>=
            either
              refuse
              (\(cid,lifecycle) ->
                runReaderT (handleClient mayConnectId cliType lifecycle st) $ ConstClientView conn cid))
        $ acceptConnection mayConnectId
     where

      refuse txt =
        let response = ConnectionRefused mayConnectId txt
        in sendBinaryData conn response

      associateClientId = do
        term <- gets shouldTerminate
        if term
          then
            return $ Left "Server is shutting down"
          else do
            Right <$>
              -- if we know some ClientId(s) associated to one of the mac adresses,
              -- and if one of these ClientIds is disconnected, we re-use it.
              state (\s ->
                let (ClientViews connectedCids macToCid nextId) = clientsViews s
                    mayReconnectingI =
                      Set.foldl
                        (\found mac ->
                          maybe
                            (maybe
                              -- this mac address is unknown:
                              Nothing
                              -- this mac address is known : we take the first unconnected id
                              -- associated to it, if it exsits.
                              (listToMaybe . Set.toList . flip Set.difference (Map.keysSet connectedCids))
                              $ macToCid !? mac)
                            Just
                            found)
                        Nothing
                        macs
                    ((i,lifecycle),s') = maybe
                        (let newId = nextId
                             clients = ClientViews connectedCids macToCid $ nextId + 1
                         in ((newId,NewClient), s { clientsViews = clients } ))
                        (\prevI -> ((prevI,ReconnectingClient),s))
                        mayReconnectingI
                    newMacToCid = Map.unionWith (Set.union) macToCid $ Map.fromSet (const $ Set.singleton i) macs
                in ((i,lifecycle), s' { clientsViews = (clientsViews s') { macMapping = newMacToCid }}))

    msg -> error $ "First sent message should be 'Connect'. " ++ show msg


handleClient :: (Server s)
             => Maybe (ConnectIdT s)
             -> ServerOwnership
             -> ClientLifecycle
             -> MVar (ServerState s)
             -> ReaderT ConstClientView IO ()
handleClient mayConnectId cliType lifecycle st = do
  i <- asks clientId
  let disconnectOnException :: (MonadUnliftIO m) => Text -> m () -> m ()
      disconnectOnException name action = try action >>= either
        (\(e :: SomeException) ->
            liftIO $ modifyMVar_ st $ execStateT $
              onBrokenClient name (Nothing :: Maybe (Text, Text, [Text])) e i)
        return
  conn <- asks connection
  -- To detect disconnections when communication is idle:
  void $ liftIO $ forkIO $ disconnectOnException "PingPong" $ pingPong conn $ fromSecs 1
  disconnectOnException "Handler" $ do
    modifyMVar_ st $ execStateT $ do
      addClient mayConnectId cliType
      case lifecycle of
        NewClient ->
          log "Is a new client"
        ReconnectingClient -> do
          log "Is a reconnecting client"
      onStartClient lifecycle

    forever $ liftIO (receiveData conn) >>=
      modifyMVar_ st . execStateT . logArg handleIncomingEvent'


handleIncomingEvent' :: (Server s
                       , MonadIO m, MonadState (ServerState s) m, MonadReader ConstClientView m)
                     => ClientEvent s
                     -> m ()
handleIncomingEvent' = \case
  Connect _ i _ ->
    handlerError $ "already connected : " ++ show i
  ClientAppEvt e -> handleClientEvent e
  ExitedState Excluded ->
    clientCanJoin (Proxy :: Proxy s) >>= bool
      (do
        notifyClient' $ EnterState Excluded
        publish WaitsToJoin)
      (publish Joins)
  ExitedState (Included x) ->
    clientCanTransition x >>= bool
      (return ())
      (publish StartsGame)
  OnCommand c -> case c of
    RequestApproval cmd -> case cmd of
      CustomCmd s -> do
        acceptCommand s >>= either
          (notifyClient' . CommandError cmd)
          (const $ acceptCmd $ CustomCmd s)
      AssignName suggested -> either
        (notifyClient' . CommandError cmd)
        (\valid -> checkNameAvailability valid >>= either
          (notifyClient' . CommandError cmd)
          (\approved -> do
            adjustClient' $ \cl -> cl { getName = approved }
            acceptCmd $ AssignName approved)
        ) $ checkName suggested
      AssignColor _ ->
        notifyClient' $ CommandError cmd "You cannot set an individual player color, please use 'Do PutColorSchemeCenter'"
      Says x ->
        acceptCmd $ Says x
      Leaves _ ->
        asks clientId >>= disconnect (ClientShutdown $ Right ()) -- will do the corresponding 'notifyEveryone $ RunCommand'
    Do cmd -> do
      case cmd of
        Succ x -> onDelta 1    x
        Pred x -> onDelta (-1) x
        Put x ->
          case x of
            ColorSchemeCenter color -> do
              modify' $ \s -> s { centerColor = color }
              adjustAllWithKey' $ \i cl -> cl { getColor = mkClientColorFromCenter i color }
              gets clientsMap >>=
                notifyEveryoneN' .
                  map (\(k, cl) -> RunCommand k (AssignColor $ getColor cl)) .
                  Map.assocs
            AppValue y ->
              onPut y
      publish $ Done cmd
    Report x ->
      case x of
        Get ColorSchemeCenterKey ->
          gets centerColor >>= notifyClient' . Reporting . Put . ColorSchemeCenter
        Get (AppValueKey y) ->
          getValue y <$> gets content >>= notifyClient' . Reporting . Put . AppValue

 where

  acceptCmd cmd = asks clientId >>= notifyEveryone' . flip RunCommand cmd

publish :: (MonadReader ConstClientView m, MonadIO m, Server s,
            MonadState (ServerState s) m)
        => PlayerNotif (ValueT s) (EnumValueKeyT s) -> m ()
publish a = asks clientId >>= notifyEveryone' . PlayerInfo a

makePlayerName :: (MonadIO m, MonadState (ServerState s) m)
               => ClientName Proposed
               -> m (ClientName Approved)
makePlayerName (ClientName sn) = do
  let go mayI = do
        let proposal = ClientName $ maybe sn ((<>) sn . pack . show) mayI
        checkNameAvailability proposal >>= either
          (\_ -> go $ Just $ maybe (2::Int) succ mayI)
          return
  go Nothing

checkNameAvailability :: (MonadIO m, MonadState (ServerState g) m)
                      => ClientName Proposed
                      -> m (Either Text (ClientName Approved))
checkNameAvailability (ClientName name) =
  any ((== name) . unClientName . getName) <$> gets clientsMap >>= \case
    True  -> return $ Left "Name is already taken"
    False -> return $ Right $ ClientName name

pingPong :: Connection -> Time Duration System -> IO ()
pingPong conn dt =
  go 0
 where
  go :: Int -> IO ()
  go i = do
    threadDelay $ fromIntegral $ toMicros dt
    sendPing conn $ pack $ show i
    go $ i + 1 -- it can overflow, that is ok.

addClient :: (Server s
            , MonadIO m, MonadState (ServerState s) m, MonadReader ConstClientView m)
          => Maybe (ConnectIdT s)
          -> ServerOwnership
          -> m ()
addClient connectId cliType = do
  conn <- asks connection
  i <- asks clientId
  color <- fmap (mkClientColorFromCenter i) (gets centerColor)
  realName <- makePlayerName $ extractName connectId
  notifyEveryoneN' $
    map (RunCommand i) [AssignColor color, AssignName realName] -- elts order is important for animations.

  let c = ClientView conn cliType realName color mkInitialClient

  modify' $ \ s ->
    let clients = clientsViews s
    in s { clientsViews =
            clients { views =
              Map.insert i c $ views clients } }
  serverLog $ (\strId -> colored "Add client" green <> "|" <> strId <> "|" <> showClient c) <$> showId i

  presentClients <-
    Map.map (\(ClientView _ _ n co _) -> ClientEssence n Present co) <$> gets clientsMap
  wp <- gets content
  greeters <- greetNewcomer

  notifyClientN' $
    [ ConnectionAccepted i
    , AllClients presentClients
    , OnContent wp
    ] ++
    map ServerAppEvt greeters

handlerError :: (Server s
                , MonadIO m, MonadState (ServerState s) m, MonadReader ConstClientView m)
             => String -> m ()
handlerError = error' "Handler"

error' :: (Server s
         , MonadIO m, MonadState (ServerState s) m, MonadReader ConstClientView m)
       => String -> String -> m ()
error' from msg = do
  log $ colored (pack txt) red
  notifyClient' $ ServerError txt
  error txt
 where
  txt = List.intercalate "|" [from, "error from Server", msg]

shutdown :: Server s
         => Text
         -> StateT (ServerState s) IO ()
shutdown reason = do
  serverLog $ pure $ colored "Server shutdown" red <> "|" <> colored reason (rgb 4 1 1)
  modify' $ \s -> s { shouldTerminate = True }
  Map.keysSet <$> gets clientsMap >>= mapM_ (disconnect $ ServerShutdown reason)
