{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}

module Imj.Random.Image
       ( writeRndImage
       , mkSystemRandomImage
       ) where

import           Imj.Prelude
import           Codec.Picture
import           Data.List(take, intercalate)
import           System.Random

import           Imj.Data.AlmostFloat
import           Imj.Geo.Discrete.Types
import           Imj.Profile.Result
import           Imj.Random.Util

-- TODO test others :
-- https://hackage.haskell.org/package/tf-random
-- https://hackage.haskell.org/package/splitmix

mkImageName :: SeedNumber -> String -> Size -> Maybe AlmostFloat -> String
mkImageName (SeedNumber seed) genName (Size (Length h) (Length w)) proba =
  intercalate "-" [show seed, genName, maybe "gray" show proba , show w ++ "x" ++ show h]

writeRndImage :: (PngSavable k)
              => SeedNumber -> String -> Size -> Maybe AlmostFloat -> Image k -> IO ()
writeRndImage seed name sz proba =
  writePng (mkImageName seed name sz proba ++ ".png")

mkSystemRandomImage :: SeedNumber -> Size -> AlmostFloat -> IO (Image Word8)
mkSystemRandomImage (SeedNumber seed) (Size (Length h) (Length w)) proba = do
    setStdGen $ mkStdGen seed -- make test deterministic

    let limit :: Word8
        limit = round $ proba * fromIntegral (maxBound :: Word8)
    withImage -- the innerloop of 'generateImage' creates an /horizontal/ column
          w h
          (\_ _ -> take 1 <$> randomRsIO(0,maxBound :: Word8) >>= \case
            [r] -> return $
              if r < limit
                then
                  minBound :: Word8
                else
                  maxBound :: Word8
            _ -> fail "logic")
