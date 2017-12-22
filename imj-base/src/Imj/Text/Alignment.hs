-- | This module is about text alignment.

module Imj.Text.Alignment
            ( AlignmentKind(..)
            , Alignment(..)
            , mkRightAlign
            , mkCentered
            , toNextLine
            -- * Utilities
            , align
            , align'
            ) where

import           Imj.Geo.Discrete

-- TODO the reference coordinates should be part of 'Alignment'

-- | Specifies where the 'Text' should be drawn w.r.t the reference coordinates.
data AlignmentKind = Centered
                   -- ^ Draw the text centered on reference coordinates.
                   | RightAligned
                   -- ^ Draw the text left of the reference coordinates.

data Alignment = Alignment {
    _alignmentKing :: !AlignmentKind -- ^ The kind of alignment.
  , _alignmentRef :: !Coords -- ^ The reference coordinates.
}

mkRightAlign :: Coords -> Alignment
mkRightAlign = Alignment RightAligned

mkCentered :: Coords -> Alignment
mkCentered = Alignment Centered

-- | Computes starting coordinates where from we should draw a series of characters
--  of a given length, to meet the alignment constraint.
align' :: Alignment
       -> Int
       -- ^ number of characters to draw
       -> Coords
align' (Alignment a ref) count =
  let (amount, dir) = align a count
  in move amount dir ref

-- | Given a number of characters and an alignment, returns the displacement
-- that should be done relatively to the reference coordinates in order to find
-- the first character position.
align :: AlignmentKind
      -> Int
      -- ^ Count of characters
      -> (Int, Direction)
align a count =
  (amount, LEFT)
 where
  amount =
    case a of
      Centered     -> 1 + quot count 2
      RightAligned -> count

-- | Moves the reference coordinate one line down.
toNextLine :: Alignment -> Alignment
toNextLine (Alignment a pos) =
  Alignment a $ translateInDir Down pos
