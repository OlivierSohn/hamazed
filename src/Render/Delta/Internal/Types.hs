{-# OPTIONS_HADDOCK hide #-}

module Render.Delta.Internal.Types
       ( BackFrontBuffer
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

-- | Buffer types
data Back
data Front

type BackFrontBuffer = IOVector (Cell, Cell)

newtype Buffer a = Buffer (IOVector Cell)

newtype Delta = Delta (Dyn.IOVector Cell)

data ClearContext = OnAllocation
                  | OnFrame
                  deriving(Eq, Show)

-- Word64 is optimal: there is no wasted space when unboxed,
--   cf. https://wiki.haskell.org/GHC/Memory_Footprint
type Cell = Word64
