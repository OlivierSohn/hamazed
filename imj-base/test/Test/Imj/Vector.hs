module Test.Imj.Vector
         ( testVector
         ) where

import           Control.Monad(when)
import           Prelude hiding (length, read)

import           Imj.Graphics.Render.Delta.DynUnboxedVec

-- | returns 1 on success, else errors
testVector :: IO Bool
testVector = do
  mapM_ testWithCapacity [0..28]
  mapM_ testSort [0..300]
  return True

testSort :: Int -> IO Bool
testSort n = do
  --print $ "sort " ++ show n
  v <- new n

  mapM_
    (\val ->
      pushBack v (-val)
    ) [(0 :: Int)..pred n]

  -- verify values
  mapM_
    (\idx -> do
      val <- read v idx
      when (val /= (-idx)) $ error $ "wrong value " ++ show (val,-idx)
    ) [0..pred n]

  unstableSort v

  -- verify values after sort
  mapM_
    (\idx -> do
      val <- read v idx
      when (val /= (-(n-1)+idx)) $ error $ "wrong value after sort" ++ show (val,idx)
    ) [0..pred n]

  return True

testWithCapacity :: Int -> IO Bool
testWithCapacity desiredCap = do
  --print $ "capacity " ++ show desiredCap
  v <- new desiredCap
  let _m = v :: IOVector Int -- TODO is this the only way I can force the type?
  l <- length v
  when (0 /= l) $ error "initial length should be 0"
  actualCap <- capacity v
  when (actualCap /= desiredCap) $ error "desired capacity not reached"
  clear v
  capAfterClear <- capacity v
  when (capAfterClear /= desiredCap) $ error "clear should keep capacity unchanged"

  -- pushback while remaining within the vector's capacity
  mapM_
    (\val -> do
      pushBack v val
      curCap <- capacity v
      when (curCap /= desiredCap) $ error "within capacity : pushback should not reallocate"
    ) [0..pred desiredCap]

  -- the next pushback will double the capacity
  mapM_
    (\val -> do
      pushBack v val
      curCap <- capacity v
      when (curCap /= (1+2*desiredCap)) $ error "within capacity : pushback should have reallocated"
    ) [desiredCap..pred $ 2*desiredCap]

  -- verify values
  mapM_
    (\idx -> do
      val <- read v idx
      when (val /= idx) $ error "wrong value"
    ) [0..pred desiredCap]
  mapM_
    (\idx -> do
      val <- read v idx
      when (val /= idx) $ error "wrong value"
    ) [desiredCap..pred $ 2*desiredCap]

  return True
