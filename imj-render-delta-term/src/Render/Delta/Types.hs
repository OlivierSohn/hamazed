{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Render.Delta.Types
            (
             -- * Buffers
              Buffers(..)
             -- ** Policies
            , Policies(..)
            , ResizePolicy(..)
            , ClearPolicy(..)
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
            , Color8(..)
            ) where

import           Control.Exception(assert)

import           Color.Types

import           Data.IORef(IORef)
import           Data.Word(Word16)

import           Render.Delta.Internal.Types

-- | Front and back buffers sizes.
data ResizePolicy = MatchTerminalSize
                  -- ^ After each render, buffers are resized (if needed) to match
                  -- terminal size.
                  | FixedSize !(Dim Width) !(Dim Height)
                  -- ^ Buffers have a fixed size. If they are vertically
                  -- or horizontally bigger than the terminal, rendering
                  -- artefacts will be visible.
                  deriving(Show, Eq)

-- TODO allow to specify alignment constraints vs terminal :
--     horizontally = right-align | left-align | center
--     vertically   = top-align | bottom-align | center
--
-- TODO allow to specify sizes in terminal percentage:

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
                 deriving(Show, Eq)

newtype Dim a = Dim Word16 deriving(Num, Eq, Ord, Show, Real, Enum, Integral)

-- | Buffer width
data Width
-- | Buffer height
data Height
-- | Buffer size (width * height)
data Size
-- | Buffer element index
data Index
-- | Index of a row
data RowIndex
-- | Index of a column
data ColIndex

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


data Buffers = Buffers {
    _renderStateBackBuffer :: !(Buffer Back)
  , _renderStateFrontBuffer :: !(Buffer Front)
  , _buffersDrawWidth :: !(Dim Width) -- The size is stored in back and front buffers
  , _buffersDelta :: !Delta
  -- ^ The delta-buffer is used in renderFrame
  , _buffersPolicies :: !Policies
}

data Policies = Policies {
    _policiesResizePolicy :: !ResizePolicy
  , _policiesClearPolicy :: !ClearPolicy
  , _policiesClearColor :: !(Color8 Background)
} deriving(Show)
