{-# LANGUAGE NoImplicitPrelude #-}

{-- | Functions to resample a list.

(The variable names used in this documentation match the ones in the code.)

If we have an input list of length n, and a desired output length of m,
each input sample will be repeated floor(m/n) times in the output, except for the
"over-represented samples" which will be repeated 1 + floor(m/n) times.

The number of over-represented samples is m' = m - n*floor(m/n).

There are several ways to place over-represented samples:

* "Even" spread : the input interval [0.0 length] is partitionned in m' equal length intervals
whose centers, floored to the previous integer, are the overrepresented samples.

    * With an input of length 5, and 2 overrepresented samples:

             input samples:   -----

  over-represented samples:    - -

    * over-represented samples indexes are:

  for every s in [0,m'-1] : f(s) = a + floor( 0.5 + (n - 1 - a) * s / (m-1))   where a = (n/m')/2

* "Even with extremities" spread: the first and last overrepresented samples match
with an input extremity. The rest of the overrepresented samples are positionned
"regularly" in-between the first and last.

    * Example with an input of length 5, and 2 overrepresented samples:

             input samples:   -----

  over-represented samples:   -   -

    * over-represented samples indexes are:

  when m' > 1, for every s in [0,m'-1] : f(s) = floor( 0.5 + (n - 1) * s / (m'-1))

  when m' == 1, for every s in [0,m'-1] : f(s) = floor( (n - 1) / 2 )
--}
module Geo.Discrete.Resample
    ( -- | Resample "evenly with extremities".
      resample
    ) where

import           Imajuscule.Prelude

import           Data.List( length )

import           Util( replicateElements )


resample :: [a]
         -- ^ input list
         -> Int
         -- ^ length of the input (we could deduce it from the input list but in some cases
         --   we know the length of the list before it is evaluated, so it's more optimal)
         -> Int
         -- ^ length of the result
         -> [a]
resample input n m
   | assert (m >= 0) m == n = input
   | otherwise =
       let nCopiesMin = quot m n
           m' = m - (nCopiesMin * n)
           res
            | m' == 0   = replicateElements nCopiesMin input
            | otherwise = let overRepIdx = getOverRepIdx (assert (m' > 0) m') n 0
                          in  resampleRec m' n 0 (overRepIdx, 0) input nCopiesMin
       in  assert (verifyResample input m res) res


resampleRec :: Int
            -- ^ over-represented samples count
            -> Int
            -- ^ input length
            -> Int
            -- ^ current index
            -> (Int, Int)
            -- ^ (next overrepresentation index, count of overrepresented samples sofar)
            -> [a]
            -- ^ the list to be resampled
            -> Int
            -- ^ the minimum count of replications : every sample will be replicated
            --   either this amount, or this amount + 1 when distance to next overrepresentation == 0
            -> [a]
resampleRec _ _ _ _ [] _ = []
resampleRec m' n curIdx (overRepIdx, s) l@(_:_) nCopiesMin =
  let (nCopies, nextState)
  -- This commented guard was used to debug cases where the assert on the line after would fail
--        | overIdx < curIdx = error ("\noverIdx " ++ show overIdx ++ "\ncurIdx  " ++ show curIdx ++ "\nm' " ++ show m' ++ "\nn " ++ show n ++ "\ns " ++ show s)
        | assert (overRepIdx >= curIdx) overRepIdx == curIdx
                = let nextS = succ s
                      nextOverRepIdx = getOverRepIdx m' n nextS
                  in  (nCopiesMin + 1, (nextOverRepIdx, nextS))
        | otherwise = (nCopiesMin    , (overRepIdx    , s))
  in replicate nCopies (head l) ++ resampleRec m' n (succ curIdx) nextState (tail l) nCopiesMin


-- | Returns maxBound when there is no over-representation
getOverRepIdx :: Int -> Int -> Int -> Int
getOverRepIdx m' n s
  | m' >  1 = floor( 0.5 + (fromIntegral ((n - 1) * s) :: Float) / fromIntegral (m'-1))
  | m' == 1 = if s == 0
                then
                  quot n 2
                else
                  maxBound
  | otherwise = assert (m' == 0) maxBound


verifyResample :: [a]
               -- ^ the input
               -> Int
               -- ^ the number of samples
               -> [a]
               -- ^ the output
               -> Bool
verifyResample input nSamples resampled
  | nSamples == length resampled = True
  | otherwise                    = error $ "\ninput    " ++ show (length input) ++
                                           "\nnSamples " ++ show nSamples ++
                                           "\nactual   " ++ show (length resampled)
