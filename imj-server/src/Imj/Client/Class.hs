{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Imj.Client.Class
      ( ClientNode(..)
      -- * reexports
      , TQueue
      , EventsForClient
      ) where

import           Control.Concurrent.STM(TQueue)
import           Control.Monad.IO.Class(MonadIO)
import           Imj.Server.Types
import           Imj.Client.Types

-- | Client-side client representation.
class
  (ClientServer (ClientServerT a))
 =>
  ClientNode a
 where
  type ClientServerT a
  type CliEvtT a

  -- | Send a 'ClientEvent' to the server.
  sendToServer' :: (MonadIO m)
                => a -> ClientEvent (ClientServerT a) -> m ()

  -- | The queue containing events that should be handled by the client.
  serverQueue :: a -> TQueue (EventsForClient (CliEvtT a) (ClientServerT a))

  -- | Fill 'serverQueue'
  writeToClient' :: (MonadIO m)
                 => a -> EventsForClient (CliEvtT a) (ClientServerT a) -> m ()
