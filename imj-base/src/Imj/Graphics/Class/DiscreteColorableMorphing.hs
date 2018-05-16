{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Graphics.Class.DiscreteColorableMorphing
            ( DiscreteColorableMorphing(..)
            -- * Reexports
            , module Imj.Graphics.Class.DiscreteDistance
            , module Imj.Graphics.Class.Colorable
            , module Imj.Graphics.Class.DiscreteMorphing -- for haddock link
            ) where

import           Imj.Prelude

import           Control.Monad.Reader.Class(MonadReader)

import           Imj.Graphics.Color.Types
import           Imj.Graphics.Class.DiscreteDistance
import           Imj.Graphics.Class.Draw
import           Imj.Graphics.Class.Colorable
import           Imj.Graphics.Class.DiscreteMorphing

{- | Like 'DiscreteMorphing', except the
'Drawable' constraint is replaced by a 'Colorable' constraint:

Morph between /drawn/ representations of 'Colorable'.

[Drawn representation of 'Colorable' x]
The visual result of IO drawing commands induced by a 'drawUsingColor' @x@ call.

Instances should statisfy the following constraints:

* A morphing between /drawn/ representations of A and B start at the /drawn/
representation of A and ends at the /drawn/ represntation of B:

\( \forall (\, from, to)\, \in v, \forall color \)

@
    d = distance from to
    drawMorphingUsingColor from to 0 color "is the same as" drawUsingColor from color
    drawMorphingUsingColor from to d color "is the same as" drawUsingColor to   color
@

* The morphing path is composed of strictly distinct /drawings/.
* The /drawings/, when seen in rapid succession, should visually produce a
/smooth/ transformation from the first to the last /drawing/.-}
class (DiscreteDistance a, Colorable a)
      => DiscreteColorableMorphing a where

  -- | A 'Colorable' is colourless so it wouldn't know in which color to draw itself,
  -- hence here we pass a 'LayeredColor'.
  drawMorphingUsingColor :: (Draw e, MonadReader e m, MonadIO m)
                         => a -> a -> Int -> LayeredColor -> m ()
