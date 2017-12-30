-- | This module exports functions and types to handle text alignemnt.

module Imj.Graphics.Text.Alignment
            ( -- * Alignment
            {- | Text can be right-aligned, center-aligned or left-aligned. -}
              AlignmentKind(..)
            , Alignment(..)
            , mkRightAlign
            , mkCentered
            -- * Helpers
            , toNextLine
            -- * Utilities
            , align
            , align'
            ) where

import           Imj.Geo.Discrete

-- TODO the reference coordinates should be part of 'Alignment'

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
    _alignmentKing :: !AlignmentKind
    -- ^ The kind of alignment.
  , _alignmentRef :: !(Coords Pos)
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
       -> Int
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
      -> Int
      -- ^ Count of characters
      -> (Int, Direction)
align a count =
  (amount, LEFT)
 where
  amount =
    case a of
      -- for one character, centerered, there is no displacement:
      Centered     -> quot (count-1) 2
      -- for one character, right aligned, there is no displacement:
      RightAligned -> (count - 1)
      LeftAligned -> 0

-- | Moves the reference coordinate one line down.
toNextLine :: Alignment -> Alignment
toNextLine (Alignment a pos) =
  Alignment a $ translateInDir Down pos
