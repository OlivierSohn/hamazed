
module Util
    ( showListOrSingleton
    ) where

import           Imajuscule.Prelude


showListOrSingleton :: Show a => [a] -> String
showListOrSingleton [e] = show e
showListOrSingleton l   = show l
