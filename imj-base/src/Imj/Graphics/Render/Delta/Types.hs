{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Imj.Graphics.Render.Delta.Types
            (
             -- * Buffers type
              Buffers(..)
             -- ** Policies
            , Policies(..)
            , ResizePolicy(..)
            , ClearPolicy(..)
            , Dim(..)
            , BufferSize
            , BufferIndex
            , getRowCol
            , getHeight
            , xyFromIndex
            -- ** Reexported types
            , Delta
            , Scissor
            , Word16
            , Height
            , Width
            , Row
            , Col
            , IORef
            , Color8
            ) where

import           Imj.Prelude

import           Control.Exception(assert)

import           Data.IORef(IORef)
import           Data.Word(Word16)

import           Imj.Geo.Discrete.Types
import           Imj.Graphics.Class.Draw
import           Imj.Graphics.Color.Types
import           Imj.Graphics.Render.Delta.Internal.Types

-- | When and how to resize buffers.
data ResizePolicy = DynamicSize
                  -- ^ After each render, buffers are resized (if needed) to match
                  -- a size passed as argument of 'deltaFlush'.
                  | FixedSize {-# UNPACK #-} !(Dim Width) {-# UNPACK #-} !(Dim Height)
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

-- | Buffer size (width * height)
data BufferSize
-- | Buffer element index
data BufferIndex

{-# INLINE getHeight #-}
getHeight :: Dim Width -> Dim BufferSize -> Dim Height
getHeight (Dim w) (Dim sz) =
  let h = quot sz w
  in Dim $ assert (h * w == sz) h

{-# INLINE getRowCol #-}
getRowCol :: Dim BufferIndex -> Dim Width -> (Dim Col, Dim Row)
getRowCol (Dim idx) (Dim w) =
      (Dim x, Dim y)
    where
      y = idx `div` w
      x = idx - y * w

{-# INLINE xyFromIndex #-}
xyFromIndex :: Dim Width -> Dim BufferIndex -> (Dim Col, Dim Row)
xyFromIndex w idx =
  getRowCol idx w

data Buffers = Buffers {
    _renderStateBackBuffer :: {-# UNPACK #-} !(Buffer Back)
  , _renderStateFrontBuffer :: {-# UNPACK #-} !(Buffer Front)
  , _buffersDrawWidth :: {-# UNPACK #-} !(Dim Width) -- We don't store the size as it is in back and front buffers
  , _buffersDrawScissor :: !Scissor
  , _buffersDelta :: !Delta
  -- ^ The delta-buffer is used in renderFrame
  , _buffersPolicies :: {-# UNPACK #-} !Policies
}

data Policies = Policies {
    _policiesResizePolicy :: {-# UNPACK #-} !ResizePolicy
  , _policiesClearPolicy :: {-# UNPACK #-} !ClearPolicy
  , _policiesClearColor :: {-# UNPACK #-} !(Color8 Background)
} deriving(Show)
