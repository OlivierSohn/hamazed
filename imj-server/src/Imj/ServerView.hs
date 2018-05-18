{-# LANGUAGE NoImplicitPrelude #-}

module Imj.ServerView
      ( mkLocalServerView
      , mkDistantServerView
      , getServerNameAndPort
      ) where

import           Imj.Network
import           Imj.ServerView.Types

mkLocalServerView :: ServerContent values
                  -> ServerView values
mkLocalServerView = ServerView Local

mkDistantServerView :: ServerName
                    -> ServerContent values
                    -> ServerView values
mkDistantServerView n = ServerView (Distant n)


getServerNameAndPort :: ServerView values -> (ServerName, ServerPort)
getServerNameAndPort (ServerView Local (ServerContent p _)) = (ServerName "0.0.0.0", p)
getServerNameAndPort (ServerView (Distant name) (ServerContent p _)) = (name, p)
