{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Graphics.Render.Delta.Flush
    ( deltaFlush
    ) where

import           Imj.Prelude

import           Control.Monad(when)
import           Data.IORef( IORef , readIORef, writeIORef)
import           Data.Vector.Unboxed.Mutable(unsafeRead, unsafeWrite, length )

import qualified Imj.Data.Vector.Unboxed.Mutable.Dynamic as Dyn
                                (clear, pushBack)

import           Imj.Graphics.Render.Delta.Types
import           Imj.Graphics.Render.Delta.Buffers
import           Imj.Graphics.Render.Delta.Cell
import           Imj.Graphics.Render.Delta.Clear
import           Imj.Graphics.Render.Delta.Draw
import           Imj.Graphics.Render.Delta.Internal.Types
import           Imj.Timing


-- | Flushes the frame, i.e renders it.
--   Then, resizes the context if needed (see 'ResizePolicy')
--   and clears the back buffer (see 'ClearPolicy').
deltaFlush :: IORef Buffers
           -> (Delta -> Dim Width -> IO (Time Duration System, Time Duration System))
           -- ^ rendering function
           -> IO (Maybe Size)
           -- ^ get discrete size function
           -> IO (Maybe Size, Either String (Time Duration System, Time Duration System, Time Duration System))
deltaFlush ref renderFunc sizeFunc =
  readIORef ref >>= \buffers@(Buffers _ _ _ _ _ policies) ->
    shouldAdjustSize buffers sizeFunc >>= maybe
      ((,) Nothing . Right <$> render DeltaMode renderFunc buffers)
      -- We force to render everything when size changes because
      -- the terminal will have moved content on resize:
      (\(w,h) -> do
        let sz = Just $ Size (fromIntegral h) (fromIntegral w)
        createBuffers policies w h >>= either
          (\msg -> do
            void $ render DeltaMode renderFunc buffers -- draw with previous buffers
            return (sz, Left msg))
          (\b -> do
            writeIORef ref b
            initializeWithContent buffers b
            (,) sz . Right <$> render FullMode renderFunc b))

data RenderMode = DeltaMode | FullMode

-- | Note that the 'Scissor' is not taken into account here.
-- We could take it into account, if needed.
render :: RenderMode
       -> (Delta -> Dim Width -> IO (Time Duration System, Time Duration System))
       -- ^ rendering function
       -> Buffers
       -> IO (Time Duration System, Time Duration System, Time Duration System)
render mode renderFunc buffers@(Buffers (Buffer b) _ width _ d@(Delta delta) _) = do
  t1 <- getSystemTime
  case mode of
    DeltaMode -> computeDelta buffers 0
    FullMode ->
      mapM_
        (\i -> do
          v <- unsafeRead b i
          Dyn.pushBack delta $ mkIndexedCell v $ fromIntegral i)
        [0..pred $ length b]

  clearIfNeeded OnFrame buffers
  t2 <- getSystemTime

  (dtCommands, dtFlush) <- renderFunc d width
  Dyn.clear delta
  return (t1...t2, dtCommands, dtFlush)


computeDelta :: Buffers
             -> Dim BufferIndex
             -- ^ the buffer index
             -> IO ()
computeDelta b@(Buffers (Buffer backBuf) (Buffer frontBuf) _ _ (Delta delta) _) idx
  | fromIntegral idx == size = return ()
  | otherwise = do
      let i = fromIntegral idx
      valueToDisplay <- unsafeRead backBuf i
      valueCurrentlyDisplayed <- unsafeRead frontBuf i
      -- TODO skip 2 space characters with the same background colors.
      when (valueToDisplay /= valueCurrentlyDisplayed) $ do
          unsafeWrite frontBuf i valueToDisplay
          Dyn.pushBack delta $ mkIndexedCell valueToDisplay idx
      computeDelta b (succ idx)
  where
    !size = length backBuf
