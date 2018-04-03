
module Imj.Graphics.Class
  ( -- * Classes
  -- | A collection of classes representing graphical elements and their properties.
    HasLayeredColor(..)
  , Colorable(..)
  , Positionable(..)
  , Drawable(..)
  , HasRectArea(..)
  , Characters(..)
  , UncoloredTextual(..)
  -- * Reexports
  , Draw
  , RectArea
  , LayeredColor
  ) where

import Imj.Graphics.Class.Colorable
import Imj.Graphics.Class.Drawable
import Imj.Graphics.Class.HasLayeredColor
import Imj.Graphics.Class.HasRectArea
import Imj.Graphics.Class.Positionable
import Imj.Graphics.Class.UncoloredTextual
import Imj.Graphics.Class.Words
