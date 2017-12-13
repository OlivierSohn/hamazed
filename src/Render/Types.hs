{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Render.Types
            ( ResizePolicy(..)
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
            -- | Reexports
            , Word16
            , module Color.Types
            , IORef
            ) where

import           Control.Exception(assert)

import           System.Console.ANSI( Color8Code(..) )

import           Color.Types
import           Data.IORef(IORef)
import           Data.Word(Word16)


data ResizePolicy = MatchTerminalSize
                  -- ^ Buffers are sized according to terminal size, and are resized
                  --   when terminal is resized. If in doubt about which
                  --   constructor to use, use this one, it covers most use cases.
                  | FixedSize !(Dim Width) !(Dim Height)
                  -- ^ Buffers have a fixed size.

-- | Specifies when to reset the back-buffer content, and with which color
data ClearPolicy = ClearAtEveryFrame
                 -- ^ When buffers are initially allocated or resized,
                 --   and after each frame render. If in doubt about which
                 --   constructor to use, use this one, it covers most use cases.
                 | ClearOnAllocationOnly
                 -- ^ When buffers are initially allocated or resized.

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
