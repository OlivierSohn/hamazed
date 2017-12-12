
module Render.Backends.Internal.Types
       ( Buffers(..)
       , Policies(..)
       , BackFrontBuffer
       , Buffer(..)
       , Back
       , Front
       , Delta(..)
       , Cell
       , ClearContext(..)
       -- reexports
       , IORef
       ) where

import           Data.IORef( IORef )

import           Data.Word( Word64 )
import           Data.Vector.Unboxed.Mutable( IOVector )

import qualified Render.Backends.Internal.UnboxedDynamic as Dyn
                                ( IOVector )
import           Render.Types

data Buffers = Buffers {
    _renderStateBackBuffer :: !(Buffer Back)
  , _renderStateFrontBuffer :: !(Buffer Front)
  , _buffersSize :: !(Dim Size)
  , _buffersWidth :: !(Dim Width)
  , _buffersDelta :: !Delta
  -- ^ buffer used in renderFrame
  , _buffersPolicies :: !Policies
}

-- | Buffer types
data Back
data Front

data Policies = Policies {
    _policiesResizePolicy :: !ResizePolicy
  , _policiesClearPolicy :: !ClearPolicy
  , _policiesClearColor :: !ClearColor
}

type BackFrontBuffer = IOVector (Cell, Cell)

newtype Buffer a = Buffer (IOVector Cell)

newtype Delta = Delta (Dyn.IOVector Cell)

data ClearContext = OnAllocation
                  | OnFrame

-- Word64 is optimal: there is no wasted space when unboxed,
--   cf. https://wiki.haskell.org/GHC/Memory_Footprint
type Cell = Word64
-- The memory layout is such that when sorted with 'compare', the order of
-- importance of fields is (by decreasing importance) :
--     backgroundColor (8 bits)
--     foregroundColor (8 bits)
--     index in buffer (16 bits)
--     character       (32 bits)
