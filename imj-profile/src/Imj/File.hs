{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Imj.File
    ( createDirectories
    ) where

import           Imj.Prelude
import           Prelude(FilePath)
import           System.FilePath(takeDirectory)
import           System.Directory(createDirectoryIfMissing)

createDirectories :: FilePath -> IO ()
createDirectories pathWithFile = do
  let path = takeDirectory pathWithFile
  createDirectoryIfMissing True path
