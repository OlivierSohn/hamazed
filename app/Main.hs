module Main where

import           Prelude( IO
                        , return )

import           Run( run )

main :: IO ()
main = do
  _ <- run
  return ()
