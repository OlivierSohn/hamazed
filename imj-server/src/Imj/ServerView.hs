{-# LANGUAGE NoImplicitPrelude #-}

module Imj.ServerView
      ( mkLocalServerView
      , mkDistantServerView
      , getServerNameAndPort
      ) where

import           Imj.ServerView.Types
import           Imj.Server.Types


mkLocalServerView :: ServerLogs
                  -> ServerConfigT s
                  -> ServerContent (ServerContentT s)
                  -> ServerView s
mkLocalServerView l p = ServerView (Local l p)

mkDistantServerView :: ServerName
                    -> ServerContent (ServerContentT s)
                    -> ServerView s
mkDistantServerView n = ServerView (Distant n)


getServerNameAndPort :: ServerView s -> (ServerName, ServerPort)
getServerNameAndPort (ServerView (Local {}) (ServerContent p _)) = (ServerName "localhost", p)
getServerNameAndPort (ServerView (Distant name) (ServerContent p _)) = (name, p)
