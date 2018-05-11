
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}

module Imj.Space.Draw
    ( mkRenderedSpace
    , drawSpace
    ) where

import           Imj.Prelude

import           Control.Monad.Reader.Class(MonadReader)
import           Data.List(length, group, concat, mapAccumL)
import qualified Data.Vector.Unboxed as V ((!))

import           Imj.Data.Matrix.Unboxed(Matrix, getRow)
import           Imj.Graphics.Render
import           Imj.Space.Types

mkRenderedSpace :: (Material -> LayeredColor)
                -> (Material -> Glyph)
                -> Space
                -> RenderedSpace
mkRenderedSpace fColor fGlyph s@(Space mat) = RenderedSpace $ matToDrawGroups mat fColor fGlyph $ getSize s

matToDrawGroups :: Matrix Material
                -> (Material -> LayeredColor)
                -> (Material -> Glyph)
                -> Size
                -> [DrawGroup]
matToDrawGroups mat fColor fGlyph s@(Size _ cs) =
  concat $ forEachRowPure mat s $ \row accessMaterial ->
    snd $ mapAccumL
      (\col listMaterials@(material:_) ->
         let count = length listMaterials
         in (col + fromIntegral count,
             DrawGroup (Coords row col) (fColor material) (fGlyph material) count))
      (Coord 0)
      $ group $ map accessMaterial [0..fromIntegral $ pred cs]

forEachRowPure :: Matrix Material
               -> Size
               -> (Coord Row -> (Coord Col -> Material) -> b)
               -> [b]
forEachRowPure mat (Size nRows _) f =
  let rowIndexes = [0..pred $ fromIntegral nRows]
  in map (\rowIdx -> do
    let row = getRow (fromIntegral rowIdx) mat -- this is O(1)
    f rowIdx (\c -> row V.! fromIntegral c)) rowIndexes


{-# INLINABLE drawSpace #-}
drawSpace :: (Draw e, MonadReader e m, MonadIO m)
          => RenderedSpace
          -> Coords Pos
          -- ^ World upper left coordinates w.r.t terminal frame.
          -> m ()
drawSpace (RenderedSpace drawGroups) upperLeft =
  mapM_ (drawGroup upperLeft) drawGroups

{-# INLINABLE drawGroup #-}
drawGroup :: (Draw e, MonadReader e m, MonadIO m)
          => Coords Pos
          -> DrawGroup
          -> m ()
drawGroup worldCoords (DrawGroup pos colors glyph count) =
  drawGlyphs count glyph (sumCoords pos worldCoords) colors
