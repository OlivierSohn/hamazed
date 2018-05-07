{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Arg.Class
      ( Arg(..)
      ) where

import           Imj.Prelude
import           Options.Applicative(Parser)

class Arg a where
  -- | The default value returns 'Nothing', meaning that the argument has
  -- no command line representation.
  parseArg :: Maybe (Parser a)
  parseArg = Nothing

instance Arg ()
