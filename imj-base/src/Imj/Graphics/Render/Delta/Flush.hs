{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Graphics.Render.Delta.Flush
    ( deltaFlush
    ) where

import           Imj.Prelude

import           Control.Monad(when)
import           Data.IORef( IORef , readIORef, writeIORef)
import           Data.Vector.Unboxed.Mutable(read, write, length )
import           System.IO( stdout, hFlush )

import qualified Imj.Data.Vector.Unboxed.Mutable.Dynamic as Dyn
                                (clear, pushBack)

import           Imj.Graphics.Render.Delta.Types
import           Imj.Graphics.Render.Delta.Buffers
import           Imj.Graphics.Render.Delta.Cell
import           Imj.Graphics.Render.Delta.Clear
import           Imj.Graphics.Render.Delta.Draw
import           Imj.Graphics.Render.Delta.Internal.Types


-- | Flushes the frame, i.e renders it.
--   Then, resizes the context if needed (see 'ResizePolicy')
--   and clears the back buffer (see 'ClearPolicy').
deltaFlush :: IORef Buffers
           -> (Delta -> Dim Width -> IO ())
           -- ^ rendering function
           -> IO ()
deltaFlush ref renderFunc =
  readIORef ref
  >>= \buffers@(Buffers _ _ _ _ _ policies) -> do
        maySize <- shouldAdjustSize buffers
        maybe
          (render DeltaMode renderFunc buffers)
          -- We force to render everything when size changes because
          -- the terminal will have moved content on resize:
          (\sz -> do
            b <- uncurry (createBuffers policies) sz
            writeIORef ref b
            initializeWithContent buffers b
            render FullMode renderFunc b)
            maySize
  >> hFlush stdout -- TODO is flush blocking? slow? could it be async?

data RenderMode = DeltaMode | FullMode

-- | Note that the 'Scissor' is not taken into account here.
-- We could take it into account, if needed.
render :: RenderMode
       -> (Delta -> Dim Width -> IO ())
       -- ^ rendering function
       -> Buffers
       -> IO ()
render mode renderFunc buffers@(Buffers (Buffer b) _ width _ d@(Delta delta) _) = do
  case mode of
    DeltaMode -> computeDelta buffers 0
    FullMode ->
      mapM_
        (\i -> do
          v <- read b i
          Dyn.pushBack delta $ mkIndexedCell v $ fromIntegral i)
        [0..pred $ length b]

  clearIfNeeded OnFrame buffers

  renderFunc d width
  Dyn.clear delta


computeDelta :: Buffers
             -> Dim BufferIndex
             -- ^ the buffer index
             -> IO ()
computeDelta
 b@(Buffers (Buffer backBuf) (Buffer frontBuf) _ _ (Delta delta) _)
 idx
  | fromIntegral idx == size = return ()
  | otherwise = do
      let i = fromIntegral idx
      -- read from back buffer
      valueToDisplay <- read backBuf i
      -- read from front buffer
      valueCurrentlyDisplayed <- read frontBuf i
      -- if differences are found, update front buffer and push the difference
      -- in delta vector
      when (valueToDisplay /= valueCurrentlyDisplayed) $ do
          write frontBuf i valueToDisplay
          Dyn.pushBack delta $ mkIndexedCell valueToDisplay idx
      -- recurse
      computeDelta b (succ idx)
  where
    size = length backBuf
