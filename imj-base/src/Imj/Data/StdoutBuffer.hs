{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE BangPatterns #-}
module Imj.Data.StdoutBuffer
    ( Stdout
    , mkStdoutBuffer
    , addStr
    , addChar
    , flush
    ) where

import           Imj.Prelude

import qualified Codec.Binary.UTF8.String as UTF8 (encode)
import           Control.Monad.ST(RealWorld)
import           Data.ByteString.Internal as BI (ByteString(..))
import           Data.ByteString as B (putStr)
import           Data.List(length)
import           Data.Primitive.MutVar(MutVar, readMutVar, newMutVar, writeMutVar)
import qualified Data.Vector.Storable.Mutable as M (MVector(..), IOVector, new, unsafeWrite)

-- | Note that using 'Stdout' implies that you know the locale of the program will be utf8.
-- You may need to use 'setLocaleEncoding' to ensure that this is the case.
newtype Stdout = Stdout (MutVar RealWorld StdoutData)

data StdoutData = StdoutData {-# UNPACK #-} !Int {-# UNPACK #-} !(M.IOVector Word8)

-- | Creates a buffer of the given capacity.
mkStdoutBuffer :: Int -> IO Stdout
mkStdoutBuffer capacity = M.new capacity >>= fmap Stdout . newMutVar . StdoutData 0

-- | Encodes (utf8) a string and adds it to the buffer. The buffer will be flushed if needed.
--
-- The encoded string should be small enough to fit within the buffer capacity, else the function errors.
addStr :: String -> Stdout -> IO ()
addStr !str (Stdout b) = readMutVar b >>= \(StdoutData used v@(M.MVector total ptr)) -> do
  when (total < required) $
    error $ "Stdout overflow:" ++ show (str, total, required) -- we could do it in chunk instead?
  let !remains = total - used
  newUsed <- if remains < required
    then do
      B.putStr $ BI.PS ptr 0 used
      pure 0
    else
      pure used
      -- we use unsafeWrite because the bounds are already checked.
  mapM_ (uncurry (M.unsafeWrite v)) $ zip [newUsed..] l
  writeMutVar b $ StdoutData (newUsed + required) v
 where
  !l = UTF8.encode str
  !required = length l

addChar :: Char -> Stdout -> IO ()
addChar = addStr . (:[])

-- | Flushes the buffer, i.e if it is not empty, issues the corresponding 'B.putStr' call
-- and empties the buffer.
flush :: Stdout -> IO ()
flush (Stdout b) = readMutVar b >>= \(StdoutData used v@(M.MVector _ ptr)) ->
  if 0 == used
    then
      return ()
    else do
      B.putStr $ BI.PS ptr 0 used
      writeMutVar b $ StdoutData 0 v
