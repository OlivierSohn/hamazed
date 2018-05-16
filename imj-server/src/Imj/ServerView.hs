{-# LANGUAGE NoImplicitPrelude #-}

module Imj.ServerView
      ( mkLocalServerView
      , mkDistantServerView
      , getServerNameAndPort
      ) where

import           Imj.Prelude
import           Imj.ServerView.Types
import           Imj.Server.Color
import           Imj.Server.Types

mkLocalServerView :: ServerLogs
                  -> Maybe ColorScheme
                  -> ServerContent values
                  -> ServerView values
mkLocalServerView l p = ServerView (Local l p)

mkDistantServerView :: ServerName
                    -> ServerContent values
                    -> ServerView values
mkDistantServerView n = ServerView (Distant n)


getServerNameAndPort :: ServerView values -> (ServerName, ServerPort)
getServerNameAndPort (ServerView (Local {}) (ServerContent p _)) = (ServerName "0.0.0.0", p)
getServerNameAndPort (ServerView (Distant name) (ServerContent p _)) = (name, p)
