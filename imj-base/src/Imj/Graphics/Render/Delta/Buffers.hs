{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Graphics.Render.Delta.Buffers
          ( Buffers
          , IORef
          , newContext
          -- utilities
          , shouldAdjustSize
          , getBufferDimensions
          , createBuffers
          , getBufferHeight
          ) where

import           Imj.Prelude hiding(replicate, unzip)
import           Prelude hiding (replicate, unzip, length)

import           Control.Exception(assert)
import           Data.IORef( IORef , newIORef )
import           Data.Maybe( fromMaybe )
import           Data.Vector.Unboxed.Mutable( replicate, unzip, length )

import qualified Imj.Data.Vector.Unboxed.Mutable.Dynamic as Dyn (new)
import           Imj.Graphics.Color.Types
import           Imj.Graphics.Render.Delta.Types
import           Imj.Graphics.Render.Delta.Internal.Types
import           Imj.Graphics.Render.Delta.Buffers.Dimensions
import           Imj.Graphics.Render.Delta.Cells
import           Imj.Graphics.Render.Delta.DefaultPolicies
import           Imj.Graphics.UI.RectArea


-- we use IORef Buffers instead of Buffers because we want to update the size of the buffers
-- dynamically

-- Creates a context using optional policies.
newContext :: Maybe ResizePolicy
           -> Maybe ClearPolicy
           -> Maybe (Color8 Background)
           -> IO (Maybe Size)
           -> IO (Either String (IORef Buffers))
newContext mayResizePolicy mayClearPolicy mayClearColor sz = do
  let resizePolicy = fromMaybe defaultResizePolicy mayResizePolicy
      clearPolicy = fromMaybe defaultClearPolicy mayClearPolicy
      clearColor = fromMaybe defaultClearColor mayClearColor
  newContext' (Policies resizePolicy clearPolicy clearColor) sz

newContext' :: Policies
            -> IO (Maybe Size)
            -> IO (Either String (IORef Buffers))
newContext' policies@(Policies resizePolicy _ _) sz =
  getPolicyDimensions resizePolicy sz
  >>= uncurry (createBuffers policies) >>= either (return . Left) (fmap Right . newIORef)

-- | Creates buffers for given width and height, replaces 0 width or height by 1.
mkBuffers :: Dim Width
          -> Dim Height
          -> Cell
          -> IO (Either String (Buffer Back, Buffer Front, Delta, Dim Width))
mkBuffers width' height' backBufferCell =
  either
    (return . Left)
    (\(sz, width) -> do
      -- To force a first render to the whole console, we initialize the front buffer
      -- to an inexistant value.
      buf <- newBufferArray sz (backBufferCell, invalidCell)
      delta <- Dyn.new $ fromIntegral sz -- reserve the maximum possible size
      let (back, front) = unzip buf
      return $ Right (Buffer back, Buffer front, Delta delta, width))
    $ bufferSizeFromWH width' height'

getBufferDimensions :: Buffers -> (Dim Width, Dim Height)
getBufferDimensions (Buffers (Buffer back) _ width _ _ _) =
  (width, getBufferHeight width $ fromIntegral $ length back)


{-# INLINE getBufferHeight #-}
getBufferHeight :: Dim Width -> Dim BufferSize -> Dim Height
getBufferHeight (Dim w) (Dim sz) =
  let h = quot sz w
  in Dim $ assert (h * w == sz) h

shouldAdjustSize :: Buffers -> IO (Maybe Size) -> IO (Maybe (Dim Width, Dim Height))
shouldAdjustSize b@(Buffers _ _ _ _ _ (Policies resizePolicy _ _)) sizeFunc =
  (\policyDims -> if policyDims /= getBufferDimensions b
    then
      Just policyDims
    else
      Nothing) <$> getPolicyDimensions resizePolicy sizeFunc

createBuffers :: Policies -> Dim Width -> Dim Height -> IO (Either String Buffers)
createBuffers pol@(Policies _ _ clearColor) w h =
  mkBuffers w h (clearCell clearColor) >>= either
    (return . Left)
    (\(newBack, newFront, newDelta, newWidth) ->
      -- no need to clear : we initialized with the right value
      return $ Right $ Buffers newBack newFront newWidth maxRectArea newDelta pol)

-- TODO use phantom types for Cell (requires Data.Vector.Unboxed.Deriving to use newtype in vector)
newBufferArray :: Dim BufferSize -> (Cell, Cell) -> IO BackFrontBuffer
newBufferArray size = replicate (fromIntegral size)
