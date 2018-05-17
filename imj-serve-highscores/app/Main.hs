
{- | This application serves high scores.
-}

module Main where

import Imj.Game.HighScores.Server

main :: IO ()
main = serveHighScores
