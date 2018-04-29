{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Imj.Game.Hamazed.Network.Setup
      ( onDelta
      , onChangeWorldParams
      , changeWorldShape
      ) where

import           Imj.Prelude

import           Control.Monad.IO.Class(MonadIO)
import           Control.Monad.State.Strict(MonadState, state)

import           Imj.Game.Hamazed.Loop.Event.Types
import           Imj.Game.Hamazed.Network.Internal.Types
import           Imj.Game.Hamazed.Network.Types
import           Imj.Game.Hamazed.Types
import           Imj.Game.Hamazed.World.Space.Types

import           Imj.Server.Connection

onDelta :: (MonadIO m, MonadState (ServerState HamazedServerState) m)
        => Int
        -> SharedEnumerableValueKey
        -> m ()
onDelta i key = onChangeWorldParams $ \wp -> case key of
  BlockSize -> case wallDistrib wp of
    p@(WallDistribution prevSize _) ->
      let adjustedSize
           | newSize < minBlockSize = minBlockSize
           | newSize > maxBlockSize = maxBlockSize
           | otherwise = newSize -- TODO define an upper bound and use it for the slider.
           where newSize = prevSize + i
      in bool
        (Just $ wp { wallDistrib = p { blockSize' = adjustedSize } })
        Nothing $
        adjustedSize == prevSize
  WallProbability -> case wallDistrib wp of
    p@(WallDistribution _ prevProba) ->
      let adjustedProba
           | newProba < minWallProba = minWallProba
           | newProba > maxWallProba = maxWallProba
           | otherwise = newProba
           where
             newProba = wallProbaIncrements * fromIntegral (round (newProba' / wallProbaIncrements) :: Int)
             newProba' = minWallProba + wallProbaIncrements * fromIntegral nIncrements
             nIncrements = i + round ((prevProba - minWallProba) / wallProbaIncrements)
      in bool
        (Just $ wp { wallDistrib = p { wallProbability' = adjustedProba } })
        Nothing $
        adjustedProba == prevProba

onChangeWorldParams :: (MonadIO m, MonadState (ServerState HamazedServerState) m)
                    => (WorldParameters -> Maybe WorldParameters)
                    -> m ()
onChangeWorldParams f =
  state (\s ->
    let mayNewParams = f prevParams
        prevParams = worldParameters $ unServerState s
    in (mayNewParams
      , maybe id (\newParams -> mapState (\s' -> s' { worldParameters = newParams })) mayNewParams s))
    >>= maybe (return ()) onChange
 where
  onChange p = do
    notifyEveryone $ OnWorldParameters p
    requestWorld

changeWorldShape :: WorldShape -> WorldParameters -> Maybe WorldParameters
changeWorldShape d p =
  bool (Just $ p { worldShape = d }) Nothing $ d == worldShape p
