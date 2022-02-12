{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}

module Imj.Random.MWC.Image
       ( mkMWC256Image
       , mkMWC256ImageGray
       , mkMWC256ImageGray'
       , mkMWC256ImageRGB
       ) where

import           Imj.Prelude

import           Codec.Picture
import           Data.Bits(shiftR)
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Vector.Storable as S
import qualified Data.Vector.Storable.Mutable as MS
import           Data.Word(Word32)
import           System.Random.MWC(uniform)

import           Imj.Space.Types

import           Imj.Data.AlmostFloat
import           Imj.Profile.Result
import           Imj.Random.MWC.Util
import           Imj.Space
import           Imj.Util

mkMWC256Image :: SeedNumber -> Size -> AlmostFloat -> IO (Image Word8)
mkMWC256Image seed sz@(Size (Length h) (Length w)) proba =
  withNumberedSeeds genImg (pure seed)
 where

  nBlocks = area sz

  genImg (gen:|[]) = do
    mv <- MS.unsafeNew nBlocks
    _ <- fillSmallVector gen proba mv -- this cuts a random Word32 in 4 Word8, then checks for probability
    v <- S.unsafeFreeze mv
    return $ generateImage
      (\i j ->
        case  v S.! (i + j*w) of
          MaterialAndKey 0xFFFF -> minBound :: Word8
          MaterialAndKey _ -> maxBound :: Word8)
      w h
  genImg _ = error "logic"


mkMWC256ImageGray :: SeedNumber -> Size -> IO (Image Word8)
mkMWC256ImageGray seed sz@(Size (Length h) (Length w)) =
  withNumberedSeeds genImg (pure seed)
 where

  nBlocks = area sz

  genImg (gen:|[]) = do
    mv <- MS.unsafeNew $ ceilToMultiple 4 nBlocks
    forM_ [0..quot nBlocks 4 - 1] $ \i -> do
      w32 <- uniform gen :: IO Word32
      let w1 = fromIntegral w32 :: Word8
          w2 = fromIntegral (w32 `shiftR` 8) :: Word8
          w3 = fromIntegral (w32 `shiftR` 16) :: Word8
          w4 = fromIntegral (w32 `shiftR` 24) :: Word8
      forM_ (zip [4*i..] [w1,w2,w3,w4]) $ uncurry (MS.unsafeWrite mv)
    v <- S.unsafeFreeze mv
    return $ generateImage
      (\i j -> v S.! (i + j*w))
      w h
  genImg _ = error "logic"


mkMWC256ImageGray' :: SeedNumber -> Size -> IO (Image Word8)
mkMWC256ImageGray' seed sz@(Size (Length h) (Length w)) =
  withNumberedSeeds genImg (pure seed)
 where

  nBlocks = area sz

  genImg (gen:|[]) = do
    mv <- MS.unsafeNew nBlocks
    forM_ [0..nBlocks - 1] $ \i -> do
      w32 <- uniform gen :: IO Word32
      MS.unsafeWrite mv i $ fromIntegral w32
    v <- S.unsafeFreeze mv
    return $ generateImage
      (\i j -> v S.! (i + j*w))
      w h
  genImg _ = error "logic"

mkMWC256ImageRGB :: SeedNumber -> Size -> IO (Image PixelRGB8)
mkMWC256ImageRGB seed sz@(Size (Length h) (Length w)) =
  withNumberedSeeds genImg (pure seed)
 where

  nBlocks = area sz

  genImg (gen:|[]) = do
    mv <- MS.unsafeNew nBlocks
    forM_ [0..nBlocks - 1] $ \i -> do
      w32 <- uniform gen :: IO Word32
      MS.unsafeWrite mv i w32
    v <- S.unsafeFreeze mv
    return $ generateImage
      (\i j ->
        let w32 = v S.! (i + j*w)
            w1 = fromIntegral w32 :: Word8
            w2 = fromIntegral (w32 `shiftR` 8) :: Word8
            w3 = fromIntegral (w32 `shiftR` 16) :: Word8
--            w4 = fromIntegral (w32 `shiftR` 24) :: Word8
        in PixelRGB8 w1 w2 w3)
      w h
  genImg _ = error "logic"
