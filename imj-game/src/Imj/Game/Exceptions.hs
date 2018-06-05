
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Imj.Game.Exceptions
    ( GracefulProgramEnd(..)
    , UnexpectedProgramEnd(..)
    ) where

import           Imj.Prelude
import           Control.Exception.Base(Exception(..))
import           Data.Text(unpack)

-- Note that we don't have GracefulClientEnd, because we use 'exitSuccess' in that case
-- and don't need an exception.
data GracefulProgramEnd =
    GracefulServerEnd
instance Exception GracefulProgramEnd
instance Show GracefulProgramEnd where
  show GracefulServerEnd        = withNewline "Graceful server shutdown."

data UnexpectedProgramEnd =
    UnexpectedProgramEnd !Text
  | ErrorFromServer !String
instance Exception UnexpectedProgramEnd
instance Show UnexpectedProgramEnd where
  show (UnexpectedProgramEnd s) = withNewline $ unpack s
  show (ErrorFromServer s)      = withNewline $ "An error occured in the Server: " ++ s

withNewline :: String -> String
withNewline = flip (++) "\n"
