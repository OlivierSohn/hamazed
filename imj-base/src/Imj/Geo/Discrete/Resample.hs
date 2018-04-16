{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}


module Imj.Geo.Discrete.Resample
    ( resampleWithExtremities
    ) where

import           Imj.Prelude

import           Data.List(length, replicate, take)

import           Imj.Util(replicateElements)


{- | Resamples a list, using the analogy where a list
is seen as a uniform sampling of a geometrical segment.

With a uniform sampling strategy, for an input of length \( n \), and a desired
output of length \( m \):

* /Regular/ samples are repeated \( r = \lfloor {m \over n} \rfloor \) times.
* /Over-represented/ samples are repeated \( r + 1 \) times.

If \( m' \) is the number of over-represented samples,

\[
\begin{alignedat}{2}
                  m &= r*n + m'   \\
\implies \quad   m' &= m - r*n
\end{alignedat}
\]

We can chose over-represented samples in at least two different ways:

* __Even spread__ :

    * Given a partition of the input continuous interval \( [\,0, length]\, \)
      in \( m' \) equal-length intervals, the over-represented samples are located at
      the (floored) centers of these intervals.

    * More precisely, over-represented samples indexes are:

        \[ \biggl\{ a + \Bigl\lfloor {1 \over 2} + { n-1-a \over m-1 } * s \Bigl\rfloor \mid s \in [\,0\,..\,m'-1] \;,\; a = {1 \over 2} * {n \over m'} \biggl\} \]

    * Example : for a length 5 input, and 2 over-represented samples:

    @
                 input samples:   -----

      over-represented samples:    - -
    @

* __"Even with extremities" spread__:

    * The first and last over-represented samples match
      with an input extremity. The rest of the over-represented samples are positionned
      "regularly" in-between the first and last. An exception is made when there is only one
      over-represented sample : in that case it is placed in the middle.

    * More precisely, over-represented samples indexes are:

        \[ if \; m' == 1 : \biggl\{ \Bigl\lfloor {n-1 \over 2} \Bigl\rfloor \biggl\} \]

        \[ otherwise : \biggl\{  \Bigl\lfloor {1 \over 2} + {n-1 \over m'-1}*s \Bigl\rfloor \mid s \in [\,0,m'-1]\, \biggl\} \]

    * Example : for a length 5 input, and 2 over-represented samples:

    @
                 input samples:   -----

      over-represented samples:   -   -
    @

        /As its name suggests, this function uses the "even with extremities" spread./

        /For clarity, the variable names used in the code match the ones in the documentation./
-}
resampleWithExtremities :: [a]
                        -- ^ Input
                        -> Int
                        -- ^ \( n \) : input length. If input is a finite list,
                        -- it is expected that \( 0 <= n <= \) @length input@
                        -> Int
                        -- ^ \( m \) : output length. It is expected that \( 0 <= m \).
                        -> [a]
                        -- ^ Output :
                        --
                        -- * when \( m < n \), it is a /downsampled/ version of the input,
                        -- * when \( m > n \), it is an /upsampled/ version of the input.
resampleWithExtremities input' n m
   | assert (m >= 0) m == n = input
   | otherwise =
       let r = quot m n
           m' = m - (r * n)
           res
            | m' == 0   = replicateElements r input
            | otherwise = let overRepIdx = getOverRepIdx (assert (m' > 0) m') n 0
                          in  resampleRec m' n 0 (overRepIdx, 0) input r
       in  assert (verifyResample input m res) res
  where
    input = take n input'


resampleRec :: Int
            -- ^ over-represented samples count
            -> Int
            -- ^ \( n \) : input length.
            -> Int
            -- ^ current index
            -> (Int, Int)
            -- ^ (next overrepresentation index, count of over-represented samples sofar)
            -> [a]
            -- ^ the list to be resampled
            -> Int
            -- ^  \( r = floor(m/n) \) : every sample will be replicated
            --   \( r \) times, or \( r + 1 \) times if distance to next overrepresentation == 0
            -> [a]
resampleRec _ _ _ _ [] _ = []
resampleRec m' n curIdx (overRepIdx, s) (v:vs) r =
  let (nCopies, nextState)
-- uncomment the following line to debug cases where the assert on the line after would fail:
--        | overIdx < curIdx = error ("\noverIdx " ++ show overIdx ++ "\ncurIdx  " ++ show curIdx ++ "\nm' " ++ show m' ++ "\nn " ++ show n ++ "\ns " ++ show s)
        | assert (overRepIdx >= curIdx) overRepIdx == curIdx
                = let nextS = s + 1
                      nextOverRepIdx = getOverRepIdx m' n nextS
                  in  (r + 1, (nextOverRepIdx, nextS))
        | otherwise = (r    , (overRepIdx    , s))
  in replicate nCopies v ++ resampleRec m' n (curIdx + 1) nextState vs r


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
