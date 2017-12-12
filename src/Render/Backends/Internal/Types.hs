
module Render.Backends.Internal.Types
       ( Buffers(..)
       , BackFrontBuffer
       , Buffer(..)
       , Back
       , Front
       , Delta(..)
       -- reexports
       , IORef

       ) where

import           Data.IORef( IORef )

import           Data.Word( Word16 )
import           Data.Vector.Unboxed.Mutable( IOVector )

import           Render.Backends.Internal.BufferCell
import qualified Render.Backends.Internal.UnboxedDynamic as Dyn
                                ( IOVector )
import           Render.Types

type BackFrontBuffer = IOVector (Cell, Cell)

newtype Buffer a = Buffer (IOVector Cell)

newtype Delta = Delta (Dyn.IOVector Cell)

-- | Buffer types
data Back
data Front

data Buffers = Buffers {
    _renderStateBackBuffer :: !(Buffer Back)
  , _renderStateFrontBuffer :: !(Buffer Front)
  , _buffersSize :: !Word16
  , _buffersWidth :: !Word16
  , _buffersDelta :: !Delta
  , _bufferSizePolicy :: !(Maybe RenderSize)
  -- ^ buffer used in renderFrame
}
