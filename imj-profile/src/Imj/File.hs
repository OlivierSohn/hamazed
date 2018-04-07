{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE LambdaCase #-}

module Imj.File
    ( createDirectories
    , renameDirectoryIfExists
    ) where

import           Imj.Prelude
import           Prelude(FilePath)
import           Data.Time.Format(iso8601DateFormat, formatTime, defaultTimeLocale)
import           System.FilePath(takeDirectory)
import           System.Directory(createDirectoryIfMissing, doesDirectoryExist, renameDirectory, getModificationTime)

createDirectories :: FilePath -> IO ()
createDirectories p =
  createDirectoryIfMissing True $ takeDirectory p

renameDirectoryIfExists :: FilePath -> IO ()
renameDirectoryIfExists path =
  doesDirectoryExist path >>= \case
    False -> return ()
    True -> getModificationTime path >>= flip go Nothing . formatTime defaultTimeLocale (iso8601DateFormat $ Just "%H:%M:%S")
 where
  go :: String -> Maybe Int -> IO ()
  go datetime maySuffix =
    doesDirectoryExist candidate >>= \case
      False -> renameDirectory path candidate
      True -> go datetime $ Just $ maybe 0 succ maySuffix -- could happen if we're really not lucky
   where
    candidate = path <> "-" <> datetime <> maybe "" ((<>) "." . show) maySuffix
