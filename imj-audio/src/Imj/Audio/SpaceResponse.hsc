{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CPP                      #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Imj.Audio.SpaceResponse
      ( SpaceResponse(..)
      , mkEmptySpaceResponse
      ) where

import           Foreign
import           Data.Data(Data(..))
import           Data.Int(Int32)
import           GHC.Generics(Generic(..))

#include "c.h"

data SpaceResponse  = SpaceResponse {
    countChannels :: !Int32
  , sampleSize :: !Int32
  , sizeInFrames :: !Int32
  , sampleRate :: !Int32
  , lengthInSeconds :: !Float
} deriving (Generic, Show, Data, Eq, Ord)

instance Storable SpaceResponse where
    sizeOf    _ = #{size spaceResponse_t}
    alignment _ = #{alignment spaceResponse_t}

    poke p spaceResponse = do
      #{poke spaceResponse_t, nChannels} p $ countChannels spaceResponse
      #{poke spaceResponse_t, sampleSize} p $ sampleSize spaceResponse
      #{poke spaceResponse_t, nFrames} p $ sizeInFrames spaceResponse
      #{poke spaceResponse_t, sampleRate} p $ sampleRate spaceResponse
      #{poke spaceResponse_t, lengthInSeconds} p $ lengthInSeconds spaceResponse

    peek p = do
      a <- #{peek spaceResponse_t, nChannels} p
      b <- #{peek spaceResponse_t, sampleSize} p
      c <- #{peek spaceResponse_t, nFrames} p
      d <- #{peek spaceResponse_t, sampleRate} p
      e <- #{peek spaceResponse_t, lengthInSeconds} p
      return $ SpaceResponse a b c d e

mkEmptySpaceResponse :: SpaceResponse
mkEmptySpaceResponse = SpaceResponse 0 0 0 0 0
