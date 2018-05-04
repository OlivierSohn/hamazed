{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}

module Imj.Network
    ( ClientName(..), unClientName
    )
    where

import           Imj.Prelude

newtype ClientName = ClientName Text
  deriving(Generic, Show, Binary, Eq, NFData)
unClientName :: ClientName -> Text
unClientName (ClientName t) = t
