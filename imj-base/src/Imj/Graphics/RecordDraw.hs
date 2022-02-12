
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}

module Imj.Graphics.RecordDraw
    ( -- * Record drawing commands for later replay
      RecordDraw
    , mkZeroRecordDraw -- for use as a starting point in a morphable animation
    , mkRecordDraw
    , finalizeRecord
    , drawRecord
    ) where

import           Imj.Prelude
import           Prelude(length)

import           Control.Monad.Reader.Class(asks)
import           Data.IORef(IORef, newIORef, readIORef, modifyIORef')

import           Imj.Geo.Discrete.Types
import           Imj.Graphics.Class.Canvas
import           Imj.Graphics.Class.DiscreteDistance
import           Imj.Graphics.Class.DiscreteMorphing
import           Imj.Graphics.Class.Positionable
import           Imj.Graphics.Font

newtype RecordDrawData = RecordDrawData [(Coords Pos, Glyph, LayeredColor)]
data RecordDraw = RecordDraw {
    _countOperations :: !(Maybe Int)
  , _operations :: !(IORef RecordDrawData)
} deriving(Generic)
instance Show RecordDraw where
  show (RecordDraw i _) = show ("RecordDraw",i)
instance Draw RecordDraw where
  fill' _ _ _ = undefined
  -- | Writes operations in reversed order
  drawGlyph' (RecordDraw Nothing r) g p c =
    liftIO $ modifyIORef' r $ \(RecordDrawData l) -> RecordDrawData $ (p,g,c):l
  drawGlyph' (RecordDraw (Just _) _) _ _ _ = error "logic"
  drawGlyphs' = undefined
  drawStr' r str pos color =
    mapM_
      (\(i,c) -> drawGlyph' r (textGlyph c) (move i RIGHT pos) color)
      $ zip [0..] str
  setScissor _ _ = undefined
  getScissor' _ = undefined
instance Canvas RecordDraw where
  getTargetSize' _ = undefined
  onTargetChanged' _ = undefined
instance Drawable RecordDraw where
  draw = void . drawRecord (-1)
instance DiscreteDistance RecordDraw where
  distance (RecordDraw mayI _) (RecordDraw mayI' _) =
    let i = fromMaybe (error "logic") mayI
        i' = fromMaybe (error "logic") mayI'
    in i' + i
instance DiscreteMorphing RecordDraw where
  drawMorphing (RecordDraw mayI r) (RecordDraw _ r') j = do
    let i = fromMaybe (error "logic") mayI
    liftIO (readIORef r) >>= \(RecordDrawData l) ->
      if j < i
        then
          void $ drawRecord' (i-j) l
        else
          liftIO (readIORef r') >>= \(RecordDrawData l') ->
            void $ drawRecord' (j-i) l'


mkRecordDraw :: IO RecordDraw
mkRecordDraw = RecordDraw Nothing <$> newIORef (RecordDrawData [])

finalizeRecord :: RecordDraw -> IO RecordDraw
finalizeRecord (RecordDraw _ r) = do
  modifyIORef' r $ \(RecordDrawData l) -> RecordDrawData $ reverse l
  readIORef r >>= \(RecordDrawData l) -> return $ RecordDraw (Just $ length l) r

mkZeroRecordDraw :: IO RecordDraw
mkZeroRecordDraw = mkRecordDraw >>= finalizeRecord

{-# INLINABLE drawRecord #-}
drawRecord :: (MonadIO m
             , MonadReader e m, Draw e)
           => Int
           -- ^ How many to draw, or a strictly negative number to draw everything
           -> RecordDraw
           -> m Int
           -- ^ (How many to draw) - (How many were drawn)
drawRecord n (RecordDraw _ r) =
  liftIO (readIORef r) >>= \(RecordDrawData l) -> drawRecord' n l

drawRecord' :: (MonadIO m
              , MonadReader e m, Draw e)
            => Int
            -> [(Coords Pos, Glyph, LayeredColor)]
            -> m Int
drawRecord' 0 _ = return 0
drawRecord' k [] = return k
drawRecord' k ((p,g,c):ds) = do
  f <- asks drawGlyph'
  f g p c
  drawRecord' (pred k) ds
