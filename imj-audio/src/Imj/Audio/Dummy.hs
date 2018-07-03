{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}
-- | Dummy module to fix https://github.com/haskell/cabal/issues/944
-- but then, we have this other issue : https://github.com/haskell/cabal/issues/4215
--
-- The doc can only be generated manually, once the dependency from teh main library
-- to the convenience library has been removed.

module Imj.Audio.Dummy
      (
      ) where
