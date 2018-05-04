{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Arg.Class
      ( Arg(..)
      ) where

import           Options.Applicative(Parser)

class Arg a where
  parseArg :: Parser a
