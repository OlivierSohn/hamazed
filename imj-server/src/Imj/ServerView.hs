{-# LANGUAGE NoImplicitPrelude #-}

module Imj.ServerView
      ( mkLocalServerView
      , mkDistantServerView
      , getServerNameAndPort
      ) where

import           Imj.ServerView.Types
import           Imj.Server.Types


mkLocalServerView :: ServerLogs
                  -> param
                  -> ServerContent p
                  -> ServerView param p
mkLocalServerView l p = ServerView (Local l p)

mkDistantServerView :: ServerName
                    -> ServerContent p
                    -> ServerView param p
mkDistantServerView n = ServerView (Distant n)


getServerNameAndPort :: ServerView p c -> (ServerName, ServerPort)
getServerNameAndPort (ServerView (Local {}) (ServerContent p _)) = (ServerName "localhost", p)
getServerNameAndPort (ServerView (Distant name) (ServerContent p _)) = (name, p)
