{- | UI elements are used to draw on the screen, in a large sense.

It is worth noting that there are two types that deal with rectangular shapes:

* 'RectContainer' represents a rectangular UI container. It
contains the 'Size' of its /content/, and an upper left coordinate.

    * Being 'Colorable', it can be wrapped in a 'Colored' to gain the notion of color.
    * Its outer frame can be drawn, and morphed using 'DiscreteColorableMorphing'.

* 'RectArea' represents a rectangular area, and has no notion of /content/ like
'RectContainer'.

    * It cannot be drawn, but it is used to define scissor areas, or simply as a
    geometric representation of an area.
-}

module Imj.Graphics.UI
  (-- * RectArea type
    RectArea(..)
   -- * RectArea functions
  , mkRectArea
  , maxRectArea
  , rectAreaSize
  , contains
  , intersection
  , isEmpty
  , rectAreaCenter
    -- ** RectArea Filtering
  , Filter
  , Positive
  , Negative
  -- * RectContainer type
  , RectContainer(..)
  , translateRectContainer
  , mkRectContainerWithTotalArea
  , mkRectContainerAtDistance
  , getSideCenters
  -- * Colored Type
  , Colored(..)
  -- * Reexports
  , Colorable(..)
  , Size
  )where


import Imj.Graphics.UI.Colored
import Imj.Graphics.UI.RectArea
import Imj.Graphics.UI.RectContainer
