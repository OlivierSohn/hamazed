
module Util
    ( showListOrSingleton
    ) where


showListOrSingleton :: Show a => [a] -> String
showListOrSingleton [e] = show e
showListOrSingleton l   = show l
