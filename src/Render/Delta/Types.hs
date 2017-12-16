{-# OPTIONS_HADDOCK hide #-}

module Render.Delta.Types
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

import qualified Render.Delta.DynUnboxedVec as Dyn
                                ( IOVector )
import           Render.Types

data Buffers = Buffers {
    _renderStateBackBuffer :: !(Buffer Back)
  , _renderStateFrontBuffer :: !(Buffer Front)
  , _buffersDrawWidth :: !(Dim Width) -- the size is stored in back and front buffers
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
} deriving(Show)


type BackFrontBuffer = IOVector (Cell, Cell)

newtype Buffer a = Buffer (IOVector Cell)

newtype Delta = Delta (Dyn.IOVector Cell)

data ClearContext = OnAllocation
                  | OnFrame
                  deriving(Eq, Show)

-- Word64 is optimal: there is no wasted space when unboxed,
--   cf. https://wiki.haskell.org/GHC/Memory_Footprint
type Cell = Word64
