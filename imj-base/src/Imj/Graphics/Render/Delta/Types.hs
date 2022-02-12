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
            , Glyph
            , FontSpec
            ) where

import           Imj.Prelude

import           Data.Word(Word16)

import           Imj.Geo.Discrete.Types
import           Imj.Graphics.Class.Draw
import           Imj.Graphics.Color.Types
import           Imj.Graphics.Font
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

{-# INLINE xyFromIndex #-}
xyFromIndex :: Dim Width -> Dim BufferIndex -> (Dim Col, Dim Row)
-- 'Dim' is unsigned, the values are always >= 0 so 'quotRem' is the way to go.
xyFromIndex (Dim w) (Dim idx) = (Dim x, Dim y) where (y,x) = idx `quotRem` w

data Buffers = Buffers {
    _renderStateBackBuffer :: {-# UNPACK #-} !(Buffer Back)
  , _renderStateFrontBuffer :: {-# UNPACK #-} !(Buffer Front)
  , _buffersDrawWidth :: {-# UNPACK #-} !(Dim Width) -- We don't store the size as it is in back and front buffers
  , _buffersDrawScissor :: !Scissor
  , _buffersDelta :: !Delta
  -- ^ The delta-buffer is used in renderFrame
  , getPolicies :: {-# UNPACK #-} !Policies
}

data Policies = Policies {
    _policiesResizePolicy :: {-unpack sum-} !ResizePolicy
  , _policiesClearPolicy :: {-unpack sum-} !ClearPolicy
  , _policiesClearColor :: {-# UNPACK #-} !(Color8 Background)
} deriving(Show)
