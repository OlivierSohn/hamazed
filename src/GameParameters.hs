{-# LANGUAGE NoImplicitPrelude #-}

module GameParameters(
        GameParameters(..)
      , getGameParameters
      , WorldShape(..)
      , WallType(..)
      , RandomParameters(..)
      , Strategy(..)
      ) where

import           Imajuscule.Prelude

data GameParameters = GameParameters {
    _gameParamsWorldShape :: !WorldShape
  , _gameParamsWallTypes :: !WallType
}

data WorldShape = Square
                | Rectangle2x1

data WallType = None
              | Deterministic
              | Random RandomParameters

data Strategy = StrictlyOneComponent

data RandomParameters = RandomParameters {
    _randomWallsBlockSize :: !Int
  , _randomWallsStrategy :: !Strategy
}

--minRandomBlockSize :: Int
--minRandomBlockSize = 6 -- using 4 it once took a very long time (one minute, then I killed the process)
                       -- 6 has always been ok

getGameParameters :: IO GameParameters
getGameParameters = return $ GameParameters Square None
