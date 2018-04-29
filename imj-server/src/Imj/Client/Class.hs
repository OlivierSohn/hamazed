{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Imj.Client.Class
      ( Client(..)
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
  (Server (ServerT a))
 =>
  Client a
 where
  type ServerT a
  type CliEvtT a

  -- | Send a 'ClientEvent' to the server.
  sendToServer' :: (MonadIO m)
                => a -> ClientEvent (ServerT a) -> m ()

  -- | The queue containing events that should be handled by the client.
  serverQueue :: a -> TQueue (EventsForClient (CliEvtT a) (ServerT a))

  -- | Fill 'serverQueue'
  writeToClient' :: (MonadIO m)
                 => a -> EventsForClient (CliEvtT a) (ServerT a) -> m ()
