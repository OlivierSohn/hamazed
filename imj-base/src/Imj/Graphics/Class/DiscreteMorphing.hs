{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}

module Imj.Graphics.Class.DiscreteMorphing
        ( DiscreteMorphing(..)
          -- * Reexport
        , module Imj.Graphics.Class.DiscreteDistance
        , module Imj.Graphics.Class.Drawable
        , module Imj.Graphics.Class.Draw
        ) where

import           Imj.Prelude

import           Imj.Graphics.Class.DiscreteDistance
import           Imj.Graphics.Class.Draw
import           Imj.Graphics.Class.Drawable


{-| Morph between /drawn/ representations of 'Drawable's.

[Drawn representation of 'Drawable' x]
The visual result of IO drawing commands induced by a 'draw' @x@ call.

Instances should statisfy the following constraints:

* A morphing between /drawn/ representations of A and B starts at the /drawn/
representation of A and ends at the /drawn/ represntation of B:

\( \forall (\, from, to)\, \in v, \)

@
    d = distance from to
    drawMorphing from to 0 "is the same as" draw from
    drawMorphing from to d "is the same as" draw to
@

* The morphing path is composed of strictly distinct /drawings/.
* The /drawings/, when seen in rapid succession, should visually produce a
/smooth/ transformation from the first to the last /drawing/. -}
class (DiscreteDistance v, Drawable v)
      => DiscreteMorphing v where
  -- | Draws the morphing between the /drawn/ representations of 2 \(v\).
  drawMorphing :: (Draw e, MonadReader e m, MonadIO m)
                => v -- ^ first value
                -> v -- ^ last value
                -> Int -- ^ the current step
                -> m ()

  -- | Draws the morphing between the /drawn/ representations of several \(v\).
  {-# INLINABLE drawMorphingSuccessive #-}
  drawMorphingSuccessive :: (Draw e, MonadReader e m, MonadIO m)
                         => Successive v
                         -> Int
                         -> m ()
  drawMorphingSuccessive (Successive []) _ = return ()
  drawMorphingSuccessive (Successive [a]) _ = drawMorphing a a 0
  drawMorphingSuccessive (Successive (a:rest@(b:_))) i
    | i <= 0      = drawMorphing a a 0
    | i >= lf = drawMorphingSuccessive (Successive rest) $ i-lf
    | otherwise = drawMorphing a b i
    where lf = pred $ distance a b
