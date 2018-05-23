{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE LambdaCase #-}

module Imj.Server.Class
      ( -- * Classes
        Server(..)
      , ServerInit(..)
      , ServerCmdParser(..)
      , ServerInParallel(..)
      , ServerClientHandler(..)
      , ServerClientLifecycle(..)
      , ChatShow(..)
      -- *
      , ServerEvent(..)
      , StateValue(..)
      , PlayerNotif(..)
      , Value(..)
      , ValueKey(..)
      , Command(..)
      , ClientCommand(..)
      , ServerCommand(..)
      , ServerReport(..)
      , ServerState(..)
      , getsState
      , mapState
      , modifyState
      , modifyState'
      , modifyStateM
      , ClientViews(..)
      , ClientView(..)
      , ClientId(..)
      , ServerOwnership(..)
      , ServerLogs(..)
      , DisconnectReason(..)
      , ClientName(..)
      , unClientName
      -- * reexports
      , MonadReader
      , MonadState
      ) where

import           Imj.Prelude
import qualified Data.Binary as Bin
import           Data.Binary(Binary)
import           Data.Proxy(Proxy)
import           Data.List(unwords)
import           Data.Map(Map)
import           Control.Concurrent.MVar.Strict (MVar)
import           Control.Monad.Reader.Class(MonadReader)
import           Control.Monad.State.Strict(MonadState, gets, modify', get, put, state)
import           Data.Attoparsec.Text(Parser)
import           Data.Text.Lazy.Encoding as LazyT
import qualified Data.Text.Lazy as LazyT
import           Network.WebSockets

import           Imj.Arg.Class
import           Imj.Categorized
import           Imj.ClientView.Internal.Types
import           Imj.Graphics.Class.UIInstructions
import           Imj.Graphics.Color.Types
import           Imj.Music.Types hiding(Do)
import           Imj.Server.Internal.Types

import           Imj.Network


-- | Server initialization.
class (Show (ClientViewT s)
     , Generic (ClientViewT s)
     , NFData (ClientViewT s)
     , Arg (ServerArgsT s)
     )
     => ServerInit s where

  -- | "Server-side" client definition.
  type ClientViewT s = (r :: *) | r -> s

  type ServerArgsT s
  type ServerArgsT s = ()

  -- | Create the server.
  mkInitialState :: MonadIO m
                 => Maybe (ServerArgsT s) -> m (ValuesT s, s)

  -- | Creates the client view
  mkInitialClient :: ClientViewT s

-- | Actions running in parallel to client handlers ().
class ServerInParallel s where
  -- | Actions that will be run concurrently after server initialization.
  --
  -- 'MVar' is used to be able to avoid race conditions.
  --
  -- Typically, a game server will return actions scheduling the game execution:
  -- see 'imj-game-hamazed' for an example where when a plyer disconnects, the game
  -- is paused and other players are informed that we are waiting for one player
  -- to reconnect.
  inParallel :: [MVar (ServerState s) -> IO ()]
  inParallel = []

-- | Methods to handle a client
class (Show (ValuesT s), Generic (ValuesT s), Binary (ValuesT s), NFData (ValuesT s)
     , ChatShow (ValueT s)
     , Show (ValueKeyT s), Generic (ValueKeyT s), Eq (ValueKeyT s), Binary (ValueKeyT s), NFData (ValueKeyT s)
     , Show (ValueT s), Generic (ValueT s), Eq (ValueT s), Binary (ValueT s), NFData (ValueT s)
     , UIInstructions (ValuesT s)
     , Show (EnumValueKeyT s), Generic (EnumValueKeyT s), Eq (EnumValueKeyT s), Binary (EnumValueKeyT s), NFData (EnumValueKeyT s)
     , NFData s -- because we use Control.Concurrent.MVar.Strict
     , Show (StateValueT s), Generic (StateValueT s), Binary (StateValueT s)
     , Show (ClientEventT s), Generic (ClientEventT s), Binary (ClientEventT s)
     , Show (ConnectIdT s), Generic (ConnectIdT s), Arg (ConnectIdT s)
     , ClientNameProposal (ConnectIdT s), Binary (ConnectIdT s)
     )
     => ServerClientHandler s where

  type StateValueT s

  type ClientEventT s = (r :: *) | r -> s
  -- ^ Events sent by the client that must be handled by the server.

  type ConnectIdT s = (r :: *) | r -> s
  -- ^ Data passed in 'ClientEvent' 'Connect'.
  type ConnectIdT s = ClientName Proposed

  -- | Handle an incoming client event.
  handleClientEvent :: (MonadIO m, MonadState (ServerState s) m, MonadReader ConstClientView m)
                    => ClientEventT s
                    -> m [MVar (ServerState s) -> IO ()]
                    -- ^ Returns (optional) actions to be executed in parallel.

  -- | Returns 'Left' to disallow the command, 'Right' to allow it.
  acceptCommand :: (MonadIO m, MonadState (ServerState s) m, MonadReader ConstClientView m)
                => CustomCmdT s
                -> m (Either Text ())
  acceptCommand = fail "Please implement 'acceptCommand' if you define 'CustomCmdT'."


  type ValueKeyT s
  type ValueKeyT s = ()

  type ValueT s
  type ValueT s = ()

  type EnumValueKeyT s
  type EnumValueKeyT s = ()

  type ValuesT s = (r :: *) | r -> s
  type ValuesT s = ()

  -- | Gets a 'ValueT' from 'ValuesT' by 'ValueKeyT'
  getValue :: ValueKeyT s -> ValuesT s -> ValueT s
  getValue _ _ = error "Please implement 'getValue' when you define 'ValuesT' / 'ValueKeyT' / 'ValueT'"

  onPut :: (MonadIO m, MonadState (ServerState s) m, MonadReader ConstClientView m)
        => ValueT s -> m ()
  onPut _ = fail "Please implement 'onPut' when you define 'ValueT'"

  onDelta :: (MonadIO m, MonadState (ServerState s) m, MonadReader ConstClientView m)
          => Int
          -> EnumValueKeyT s
          -> m ()
  onDelta _ _ = fail "Please implement 'onDelta' when you define 'EnumValueKeyT'"

-- | Methods related to the lifecycle of a client. Note that client-specific infos
-- ('ClientId' and connection) are available through 'MonadReader' 'ConstClientView',
-- or passed as parameter (see 'afterClientLeft').
class ServerClientLifecycle s where
  -- | Called after the client has been added and sent the greeting events (see 'greetNewcomer').
  -- Default implementation does nothing.
  onStartClient :: (MonadIO m, MonadState (ServerState s) m, MonadReader ConstClientView m)
                => ClientLifecycle -> m ()
  onStartClient _ = return ()

  -- | Returns True if the client was included in the game being set up.
  clientCanJoin :: (MonadIO m, MonadState (ServerState s) m, MonadReader ConstClientView m)
                => Proxy s -> m Bool
  clientCanJoin _ = return True

  -- | Returns True if the client starts the game.
  clientCanTransition :: (MonadIO m, MonadState (ServerState s) m, MonadReader ConstClientView m)
                      => StateValueT s -> m Bool
  clientCanTransition _ = return True

  -- | Called after a client has disconnected (either intentionally or on connection error).
  --
  -- Default implementation does nothing.
  afterClientLeft :: (MonadIO m, MonadState (ServerState s) m)
                  => ClientId
                  -> m ()
  afterClientLeft _ = return ()

class (Show (ServerEventT s)
     , Show (CustomCmdT s)
     , Generic (ServerEventT s)
     , Generic (CustomCmdT s)
     , Eq (CustomCmdT s)
     , Binary (ServerEventT s)
     , Binary (CustomCmdT s)
     , Categorized (ServerEventT s)
     )
 =>
  Server s
 where

  type ServerEventT s
  -- ^ Events sent by the server that must be handled by the client.

  -- | These 'ServerEventT' are sent to the newly added client. They should include
  -- state information that the client needs to know to be up-to-date w.r.t the
  -- current server state.
  greetNewcomer :: (MonadIO m, MonadState (ServerState s) m)
                => m [ServerEventT s]
  greetNewcomer = return []
  -- | These 'ServerEvent' are sent to the newly added client. They should include
  -- state information that the client needs to know to be up-to-date w.r.t the
  -- current server state.
  greetNewcomer' :: (MonadIO m, MonadState (ServerState s) m)
                 => m [ServerEvent s]
  greetNewcomer' = return []

-- | Parsing of custom commands parameters
class ServerCmdParser s where

  type CustomCmdT s
  type CustomCmdT s = ()

  {- | Returns parsers for command parameters.

Commands start with a forward slash, followed by the command name and optional parameters.

The command names that are keys in the returned 'Map' must be lowercased and composed
exclusively of alphanumerical characters. They also must not overlapp with
the default command names (@name@ and @color@ at the time of this writing) else
the command will not be reachable.

The parsers returned should consider that the input has been consumed up until the beginning
of the parameters, for example:

@
/setValues 2 3 4
           ^
/setValues
          ^

'^' : parse position when this method is called
@
  -}
  cmdParsers :: Map Text (Parser (Command s))
  -- ^ the key is the command name (lowercased, using only alpha numerical characters),
  -- the value is a parser for the command /parameters/.
  cmdParsers = mempty


class Show a => ChatShow a where
  -- | Returns the 'String' to display in the chat window. The default implemention
  -- is 'show'.
  chatShow :: a -> String
  chatShow = show
instance ChatShow ()

data ServerState s = ServerState {
    serverLogs :: {-unpack sum-} !ServerLogs
  , clientsViews :: {-# UNPACK #-} !(ClientViews (ClientViewT s))
  , shouldTerminate :: {-unpack sum-} !Bool
  -- ^ Set on server shutdown
  , content :: !(ValuesT s)
  , centerColor :: {-# UNPACK #-} !(Color8 Foreground)
  -- ^ The color scheme.
  , unServerState :: !s
} deriving(Generic)
instance (ServerInit s, ServerClientHandler s) => NFData (ServerState s)

{-# INLINABLE getsState #-}
getsState :: (MonadState (ServerState s) m)
          => (s -> a) -> m a
getsState f = gets (f . unServerState)

{-# INLINE modifyState #-}
modifyState :: MonadState (ServerState s) m
            => (s -> s) -> m ()
modifyState = modify' . mapState

{-# INLINE modifyState' #-}
modifyState' :: MonadState (ServerState s) m
             => (s -> (a,s)) -> m a
modifyState' = state . mapState'

{-# INLINE modifyStateM #-}
modifyStateM :: MonadState (ServerState s) m
             => (s -> m s) -> m ()
modifyStateM f = do
  s <- get
  res <- f $ unServerState s
  put $ s { unServerState = res }

{-# INLINE mapState #-}
mapState :: (s -> s) -> ServerState s -> ServerState s
mapState f s = s { unServerState = f $ unServerState s }

{-# INLINE mapState' #-}
mapState' :: (s -> (a,s)) -> ServerState s -> (a,ServerState s)
mapState' f s =
  let (res,s') = f $ unServerState s
  in (res, s { unServerState = s' })

data ServerEvent s =
    ServerAppEvt !(ServerEventT s)
  | PlayMusic !Music
  | CommandError {-unpack sum-} !(ClientCommand (CustomCmdT s) Proposed)
                 {-# UNPACK #-} !Text
  -- ^ The command cannot be run, with a reason.
  | RunCommand {-# UNPACK #-} !ClientId
               {-unpack sum-} !(ClientCommand (CustomCmdT s) Approved)
  -- ^ The server validated the use of the command, now it must be executed.
  | Reporting {-unpack sum-} !(ServerCommand (ValueT s) (EnumValueKeyT s))
  -- ^ Response to a 'Report'.
  | PlayerInfo !(PlayerNotif (ValueT s) (EnumValueKeyT s))
               {-# UNPACK #-} !ClientId
  | ConnectionAccepted {-# UNPACK #-} !ClientId
  | ConnectionRefused !(Maybe (ConnectIdT s)) {-# UNPACK #-} !Text
  | Disconnected {-unpack sum-} !DisconnectReason
  | OnContent !(ValuesT s)
  -- ^ Sent to every newly connected client, and to all clients whenever the content changes.
  | AllClients !(Map ClientId ClientEssence)
  | EnterState {-unpack sum-} !(StateValue (StateValueT s))
  | ExitState {-unpack sum-} !(StateValue (StateValueT s))
  | Warn !Text
  | ServerError !String
  -- ^ A non-recoverable error occured in the server: before crashing, the server sends the error to its clients.
  deriving(Generic)
instance (Server s, ServerClientHandler s) => Show (ServerEvent s) where
  show = \case
    ServerAppEvt x -> show ("ServerAppEvt",x)
    PlayMusic x -> show ("PlayMusic",x)
    CommandError x y -> show ("CommandError",x, y)
    RunCommand x y -> show ("RunCommand",x, y)
    Reporting x -> show ("Reporting",x)
    PlayerInfo x y -> show ("PlayerInfo",x, y)
    ConnectionAccepted x -> show ("ConnectionAccepted",x)
    ConnectionRefused x y -> show ("ConnectionRefused",x,y)
    Disconnected x -> show ("Disconnected",x)
    OnContent x -> show ("OnContent",x)
    AllClients x -> show ("AllClients",x)
    EnterState x -> show ("EnterState",x)
    ExitState x -> show ("ExitState",x)
    ServerError x -> show ("ServerError",x)
    Warn x -> show ("Warning",x)
instance (Server s, ServerClientHandler s) => Binary (ServerEvent s)
instance (Server s, ServerClientHandler s) => WebSocketsData (ServerEvent s) where
  fromDataMessage (Text t _) =
    error $ "Text was received for ServerEvent : " ++ LazyT.unpack (LazyT.decodeUtf8 t)
  fromDataMessage (Binary bytes) = Bin.decode bytes
  fromLazyByteString = Bin.decode
  toLazyByteString = Bin.encode
  {-# INLINABLE fromDataMessage #-}
  {-# INLINABLE fromLazyByteString #-}
  {-# INLINABLE toLazyByteString #-}
instance (Server s, ServerClientHandler s) => Categorized (ServerEvent s) where
  evtCategory = \case
    PlayMusic{} -> Command'
    Reporting _ -> Chat'
    PlayerInfo _ _ -> Chat'
    ServerError _ -> Error'
    Disconnected _ -> Disconnected'
    ConnectionAccepted {} -> ConnectionAccepted'
    ConnectionRefused {} -> ConnectionRefused'
    CommandError _ _ -> Error'
    RunCommand _ _ -> WorldRequest'
    OnContent _ -> Chat'
    AllClients{} -> Chat'
    EnterState _ -> EnterState'
    ExitState _ -> ExitState'
    Warn {} -> Chat'
    ServerAppEvt e -> evtCategory e

data StateValue s =
    Excluded
    -- ^ The client is not part of the game
  | Included !s
  -- ^ The client is part of the game
  deriving(Generic, Show, Eq)
instance Binary s => Binary (StateValue s)
instance NFData s => NFData (StateValue s)

data PlayerNotif v e =
    Joins
  | WaitsToJoin
  | StartsGame
  | Done {-unpack sum-} !(ServerCommand v e)
    -- ^ The server notifies whenever a 'Do' task is finished.
  deriving(Generic, Show)
instance (Binary v, Binary e) => Binary (PlayerNotif v e)

data Command s =
    RequestApproval !(ClientCommand (CustomCmdT s) Proposed)
  -- ^ A Client asks for authorization to run a 'ClientCommand'.
  -- In response the server either sends 'CommandError' to disallow command execution or 'RunCommand' to allow it.
  | Do !(ServerCommand (ValueT s) (EnumValueKeyT s))
  -- ^ A Client asks the server to run a 'ServerCommand'.
  -- In response, the server runs the 'ServerCommand' then publishes a 'PlayerNotif' 'Done' 'ServerCommand'.
  | Report !(ServerReport (ValueKeyT s))
  -- ^ A client want to know an information on the server state. The server will answer by
  -- sending a 'Report'.
  deriving(Generic) -- Eq needed for parse tests
instance (ServerClientHandler s, Server s) => Binary (Command s)
instance (ServerClientHandler s, Server s) => Show (Command s) where
  show = \case
    RequestApproval x -> show ("RequestApproval",x)
    Do x -> show ("Do",x)
    Report x -> show ("Report",x)
instance (ServerClientHandler s, Server s) => Eq (Command s) where
  RequestApproval x == RequestApproval y = x == y
  Report x == Report y = x == y
  Do x == Do y = x == y
  _ == _ = False

-- | Commands initiated by /one/ client or the server, authorized (and in part executed) by the server,
--  then executed (for the final part) by /every/ client. The 'a' phantom type tracks if the
-- command was approved by the server or not.
data ClientCommand b a =
    CustomCmd !b
  | AssignName {-# UNPACK #-} !(ClientName a)
  | AssignColor {-# UNPACK #-} !(Color8 Foreground)
  | Says {-# UNPACK #-} !Text
  | Leaves {-unpack sum-} !(Either Text ())
  -- ^ The client shuts down. Note that clients that are 'ClientOwnsServer',
  -- will also gracefully shutdown the server.
  deriving(Generic, Show, Eq)
instance Binary b => Binary (ClientCommand b a)

-- | Commands initiated by a client, executed by the server.
data ServerCommand v e =
    Put !(Value v)
  | Succ !e
  | Pred !e
  deriving(Generic, Show, Eq) -- Eq needed for parse tests
instance (Binary v, Binary e) => Binary (ServerCommand v e)
instance (ChatShow v, Show e) => ChatShow (ServerCommand v e) where
  chatShow (Put x) = chatShow x
  chatShow (Succ x) =
    unwords ["incremented", show x]
  chatShow (Pred x) =
    unwords ["decremented", show x]

-- | Describes what the client wants to know about the server.
data ServerReport vk =
    Get !(ValueKey vk)
  deriving(Generic, Show, Eq)
instance Binary vk => Binary (ServerReport vk)

-- | Value shared by all clients.
data Value v =
    ColorSchemeCenter {-# UNPACK #-} !(Color8 Foreground)
  | AppValue !v
  deriving(Generic, Show, Eq) -- Eq needed for parse tests
instance Binary v => Binary (Value v)
instance NFData v => NFData (Value v)
instance ChatShow v => ChatShow (Value v) where
  chatShow (ColorSchemeCenter c) =
    unwords ["color scheme center:", show $ color8CodeToXterm256 c]
  chatShow (AppValue x) =
    chatShow x

-- | Identifiers of values shared by all clients.
data ValueKey vk =
    ColorSchemeCenterKey
  | AppValueKey !vk
  deriving(Generic, Eq, Show) -- Eq needed for parse tests
instance Binary vk => Binary (ValueKey vk)
instance NFData vk => NFData (ValueKey vk)
