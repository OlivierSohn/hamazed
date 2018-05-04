{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Imj.Game.Hamazed.Run
      ( run
      ) where

import           Imj.Prelude
import           Data.Proxy(Proxy(..))

import           Imj.Game.App
import           Imj.Game.Hamazed

run :: IO ()
run = runGame (Proxy :: Proxy HamazedGame)
