
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}

module Imj.Game.Hamazed.World.Space.Draw
    ( materialColor
    , materialGlyph
    , mkRenderedSpace
    , drawSpace
    -- * Reexports
    , module Imj.Graphics.Render
    ) where

import           Imj.Prelude

import           Control.Monad.IO.Class(MonadIO)
import           Control.Monad.Reader.Class(MonadReader)
import           Data.List(length, group, concat, mapAccumL)
import qualified Data.Vector.Unboxed as V ((!))

import           Imj.Data.Matrix.Unboxed(Matrix, getRow)
import           Imj.Game.Hamazed.Color
import           Imj.Game.Hamazed.World.Space.Types
import           Imj.Graphics.Class.Positionable
import           Imj.Graphics.Font
import           Imj.Graphics.Render

mkRenderedSpace :: Space -> RenderedSpace
mkRenderedSpace s@(Space mat) = RenderedSpace $ matToDrawGroups mat $ getSize s

matToDrawGroups :: Matrix Material -> Size -> [DrawGroup]
matToDrawGroups mat s@(Size _ cs) =
  concat $ forEachRowPure mat s $ \row accessMaterial ->
    snd $ mapAccumL
      (\col listMaterials@(material:_) ->
         let count = length listMaterials
         in (col + fromIntegral count,
             DrawGroup (Coords row col) (materialColor material) (materialGlyph material) count))
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

{-# INLINE materialColor #-}
materialColor :: Material -> LayeredColor
materialColor = \case
  Wall -> wallColors
  Air  -> airColors

{-# INLINE materialGlyph #-}
materialGlyph :: Material -> Glyph
materialGlyph = gameGlyph . (\case
  Wall -> 'Z'
  Air  -> ' ')


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
