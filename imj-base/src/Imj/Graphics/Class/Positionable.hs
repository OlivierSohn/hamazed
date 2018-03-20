{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}

module Imj.Graphics.Class.Positionable
            ( Positionable(..)
            , HasReferencePosition(..)
            , GeoTransform(..)
              -- * Alignment
            , AlignmentKind(..)
            , Alignment(..)
            , mkRightAlign
            , mkCentered
            -- * Helpers
            , toNextLine
            -- * Utilities
            , align
            , align'
            ) where

import           Imj.Prelude

import           Control.Monad.Reader.Class(MonadReader, asks)
import qualified Data.Text as Text(length)
import qualified Data.List as List(length)

import           Imj.Geo.Discrete.Types

import           Imj.Graphics.Class.Draw
import           Imj.Graphics.Color
import           Imj.Graphics.Font
import           Imj.Graphics.UI.RectArea

--------------------------------------------------------------------------------
-- Positionable
--------------------------------------------------------------------------------

-- | A 'Positionable' is a graphical element that knows how to draw itself
-- except for its positions.
class Positionable a where
  -- | Draw at a given position.
  drawAt :: (Draw e, MonadReader e m, MonadIO m)
         => a -> Coords Pos -> m ()

  -- | Return the width of the 'Positionable',
  -- it is used by 'drawAligned'.
  width :: a -> Length Width
  height :: a -> Length Height

  {-# INLINABLE drawAligned #-}
  -- | Draw 'Positionable' aligned w.r.t alignment and reference coordinates.
  --
  -- Return an 'Alignment' where the reference coordinate of the input 'Alignment'
  -- was projected on the next line.
  drawAligned :: (Draw e, MonadReader e m, MonadIO m)
              => a -> Alignment -> m Alignment
  drawAligned p alignment = do
    let w = width p
        pos = align' alignment w
    drawAt p pos
    return $ toNextLine alignment

  {-# INLINABLE drawAligned_ #-}
  -- | Draw 'Positionable' aligned w.r.t alignment and reference coordinates.
  drawAligned_ :: (Draw e, MonadReader e m, MonadIO m)
               => a -> Alignment -> m ()
  drawAligned_ p al =
    void $ drawAligned p al

instance Positionable Text where
  drawAt txt pos = do
    d <- asks drawTxt'
    d txt pos whiteOnBlack
  width = fromIntegral . Text.length
  height _ = 1
  {-# INLINABLE drawAt #-}
  {-# INLINABLE width #-}
  {-# INLINABLE height #-}

instance Positionable ([] Char) where
  drawAt txt pos = do
    d <- asks drawStr'
    d txt pos whiteOnBlack
  width = fromIntegral . List.length
  height _ = 1
  {-# INLINABLE drawAt #-}
  {-# INLINABLE width #-}
  {-# INLINABLE height #-}

instance Positionable Char where
  drawAt c pos = do
    d <- asks drawGlyph'
    d (textGlyph c) pos whiteOnBlack
  width _ = 1
  height _ = 1
  {-# INLINABLE drawAt #-}
  {-# INLINABLE width #-}
  {-# INLINABLE height #-}

instance Positionable Glyph where
  drawAt g pos = do
    d <- asks drawGlyph'
    d g pos whiteOnBlack
  width _ = 1
  height _ = 1
  {-# INLINABLE drawAt #-}
  {-# INLINABLE width #-}
  {-# INLINABLE height #-}


--------------------------------------------------------------------------------
-- GeoTransform / HasReferencePosition
--------------------------------------------------------------------------------

class GeoTransform a where
  transform :: (Coords Pos -> Coords Pos) -> a -> a

class (GeoTransform a) => HasReferencePosition a where
  getPosition :: a -> Coords Pos

  {-# INLINABLE setPosition #-}
  setPosition :: Coords Pos -> a -> a
  setPosition target a =
    let origin = getPosition a
        offset = diffCoords target origin
    in transform (sumCoords offset) a

  {-# INLINE translate #-}
  -- | Translates the whole object using 'GeoTransform'
  translate :: Coords Pos -> a -> a
  translate offset =
    transform (sumCoords offset)

  {-# INLINE translateInDir #-}
  -- | Translate of 1 step in a given direction.
  translateInDir :: Direction -> a -> a
  translateInDir dir =
    translate $ coordsForDirection dir

  {-# INLINE move #-}
  move :: Int
       -- ^ Take that many steps
       -> Direction
       -- ^ In that direction
       -> a
       -> a
  move t dir = translate $ multiply t $ coordsForDirection dir

instance HasReferencePosition (Coords Pos) where
  getPosition = id
  {-# INLINE getPosition #-}
instance GeoTransform (Coords Pos) where
  transform f = f
  {-# INLINE transform #-}

instance HasReferencePosition (RectArea a) where
  getPosition (RectArea ul _) = ul
  {-# INLINE getPosition #-}
instance GeoTransform (RectArea a) where
  transform f (RectArea ul lr) = RectArea (transform f ul) (transform f lr)
  {-# INLINE transform #-}



--------------------------------------------------------------------------------
-- Alignment
--------------------------------------------------------------------------------


-- | Specifies where the 'Text' is w.r.t the reference coordinates.
data AlignmentKind = Centered
                   {- ^ /Centered/ on reference coordinates, favoring the 'RIGHT'
                   side in case of ambiguity:

@
  1
  12
 123
 1234
  ^
@ -}
                   | RightAligned
                   {- ^ /Left/ of the reference coordinates, including it:

@
   1
  12
 123
1234
   ^
@ -}
                   | LeftAligned
                   {- ^ /Right/ of the reference coordinates, including it:

@
1
12
123
1234
^
@
-}

data Alignment = Alignment {
    _alignmentKind :: {-unpack sum-} !AlignmentKind
    -- ^ The kind of alignment.
  , _alignmentRef :: {-# UNPACK #-} !(Coords Pos)
    -- ^ The reference coordinates.
}

mkRightAlign :: Coords Pos
             -- ^ The text will be written left of these coordinates.
             -> Alignment
mkRightAlign = Alignment RightAligned

mkCentered :: Coords Pos
           -- ^ The text will be centered on these coordinates.
           -> Alignment
mkCentered = Alignment Centered

-- | Computes starting coordinates where from we should draw a series of characters
--  of a given length, to meet the alignment constraint.
align' :: Alignment
       -> Length Width
       -- ^ number of characters to draw
       -> Coords Pos
align' (Alignment a ref) count =
  let (amount, dir) = align a count
  in move amount dir ref

{- | Given a number of characters and an alignment, returns the displacement
that should be done relatively to the reference coordinates in order to find
the first character 'Coords'.

For 'Centered', when we have an /even/ count of characters to draw, we
(somewhat arbitrarily) chose to favor the 'RIGHT' 'Direction', as illustrated
here where @^@ indicates where the reference 'Coords' is:

@
   1
   12
  123
  1234
   ^
@

Note that this choice impacts the implementation of
'Imj.Graphics.UI.RectContainer.getSideCentersAtDistance'.
-}
align :: AlignmentKind
      -> Length Width
      -> (Int, Direction)
align a count =
  (fromIntegral amount, LEFT)
 where
  amount =
    case a of
      -- for one character, centerered, there is no displacement:
      Centered     -> quot (count-1) 2
      -- for one character, right aligned, there is no displacement:
      RightAligned -> count - 1
      LeftAligned -> 0

-- | Moves the reference coordinate one line down.
toNextLine :: Alignment -> Alignment
toNextLine (Alignment a pos) =
  Alignment a $ translateInDir Down pos
