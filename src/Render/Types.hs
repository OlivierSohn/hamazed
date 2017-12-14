{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Render.Types
            ( -- ** Policies
              ResizePolicy(..)
            , ClearPolicy(..)
            , ClearColor
            , Dim(..)
            , Size
            , Width
            , Height
            , Index
            , RowIndex
            , ColIndex
            , getRowCol
            , getHeight
            -- ** Reexported types
            , Word16
            , IORef
            ) where

import           Control.Exception(assert)

import           System.Console.ANSI( Color8Code(..) )

import           Data.IORef(IORef)
import           Data.Word(Word16)


-- | Specifies /when/ to resize the context, and how to compute the new size.
data ResizePolicy = MatchTerminalSize
                  -- ^ The context is resized, if needed, after each call to 'flush'
                  --   and on context creation. The target size is the current terminal
                  --   size.
                  | FixedSize !(Dim Width) !(Dim Height)
                  -- ^ The context has a fixed size. Note that if the user of the program resizes
                  --   the console to a size smaller than the context size, rendering
                  --   artefacts will be visible. Consider using 'MatchTerminalSize'
                  --   if this is a concern to your application.

-- | Specifies /when/ to clear the back-buffer.
data ClearPolicy = ClearAtEveryFrame
                 -- ^ Clears the back-buffer after allocation
                 --   and after each frame render.
                 | ClearOnAllocationOnly
                 -- ^ Clears the back-buffer after allocation only.
                 --   Typically, you will use it if at every frame you draw at every screen location.
                 --   If you don't redraw every screen location at every frame, it is safer
                 --   to use 'ClearAtEveryFrame', else you will see previous frame elements
                 --   in the rendered frame (unless you intend to have this behaviour).

type ClearColor = Color8Code

newtype Dim a = Dim Word16 deriving(Num, Eq, Ord, Show, Real, Enum, Integral)

data Width  -- buffer width
data Height -- buffer height
data Size   -- buffer size (width * height)
data Index  -- buffer element index
data RowIndex -- index of a row
data ColIndex -- index of a column

{-# INLINE getHeight #-}
getHeight :: Dim Width -> Dim Size -> Dim Height
getHeight (Dim w) (Dim sz) =
  let h = quot sz w
  in Dim $ assert (h * w == sz) h

{-# INLINE getRowCol #-}
getRowCol :: Dim Index -> Dim Width -> (Dim ColIndex, Dim RowIndex)
getRowCol (Dim idx) (Dim w) =
      (Dim x, Dim y)
    where
      y = idx `div` w
      x = idx - y * w
