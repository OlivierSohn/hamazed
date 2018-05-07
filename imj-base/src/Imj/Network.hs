{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Imj.Network
    ( ClientName(..), unClientName
    , Proposed, Approved
    , ClientNameSuggestion(..)
    )
    where

import           Imj.Prelude
import           Data.String(IsString(..))
import           Options.Applicative(str, option, long, help, readerError)

import           Imj.Arg.Class

data Proposed
data Approved

newtype ClientName a = ClientName Text
  deriving(Generic, Show, Binary, Eq, NFData, IsString)
unClientName :: ClientName a -> Text
unClientName (ClientName t) = t

class ClientNameSuggestion a where
  extractName :: a -> ClientName Proposed

instance ClientNameSuggestion () where
  extractName = const $ ClientName "void"

instance ClientNameSuggestion (ClientName a) where
  extractName = ClientName . unClientName


instance Arg (ClientName Proposed) where
  parseArg =
    Just $
      option connectId
        (  long "connectId"
        <> help (
        "[Client] The connection identifier used to connect to the server.")
        )
   where

    connectId =
      str >>= \case
        [] -> readerError $ "Encountered an empty connection id."
        name -> return $ fromString name
