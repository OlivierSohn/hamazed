{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}

module Imj.Server.Types
      ( clientsMap
      , mkServerState
      , ClientEvent(..)
      , ServerArgs(..)
      -- * reexport
      , ServerLogs(..)
      , DisconnectReason(..)
      , ClientLifecycle(..)
      ) where

import           Imj.Prelude
import qualified Data.Binary as Bin
import qualified Data.Map.Strict as Map
import           Data.Map.Strict(Map)
import           Data.Set(Set)
import           Data.Text.Lazy.Encoding as LazyT
import qualified Data.Text.Lazy as LazyT
import           Network.WebSockets

import           Imj.ClientView.Types
import           Imj.Graphics.Color
import           Imj.Network
import           Imj.Server.Color
import           Imj.Server.Internal.Types
import           Imj.Server.Class

data ServerArgs s = ServerArgs !ServerLogs !(Maybe ColorScheme) !(Maybe (ServerArgsT s))

data ClientEvent s =
    ClientAppEvt !(ClientEventT s)
  | Connect !(Set MAC) !(Maybe (ConnectIdT s)) {-unpack sum-} !ServerOwnership
  | ExitedState {-unpack sum-} !(StateValue (StateValueT s))
  | OnCommand !(Command s)
  | SequenceOfCliEvts [ClientEvent s]
  -- ^ where the first elements in the list should be handled first
  deriving(Generic)
instance (Server s, ServerClientHandler s) => Show (ClientEvent s) where
  show = \case
    ClientAppEvt x -> show ("ClientAppEvt",x)
    Connect x y z -> show ("Connect",x,y,z)
    ExitedState x -> show ("ExitedState",x)
    OnCommand x -> show ("OnCommand",x)
    SequenceOfCliEvts l -> show ("Sequence", show l)
instance (Server s, ServerClientHandler s) => Binary (ClientEvent s)
instance (Server s, ServerClientHandler s) => WebSocketsData (ClientEvent s) where
  fromDataMessage (Text t _) =
    error $ "Text was received for ClientEvent : " ++ LazyT.unpack (LazyT.decodeUtf8 t)
  fromDataMessage (Binary bytes) = Bin.decode bytes
  fromLazyByteString = Bin.decode
  toLazyByteString = Bin.encode
  {-# INLINABLE fromDataMessage #-}
  {-# INLINABLE fromLazyByteString #-}
  {-# INLINABLE toLazyByteString #-}

mkServerState :: ServerLogs -> Color8 Foreground -> ValuesT s -> s -> ServerState s
mkServerState logs color c s =
  ServerState logs (ClientViews Map.empty Map.empty (ClientId 0)) False c color s

{-# INLINE clientsMap #-}
clientsMap :: ServerState s -> Map ClientId (ClientView (ClientViewT s))
clientsMap = views . clientsViews
