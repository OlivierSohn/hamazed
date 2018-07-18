{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Geo.Discrete.Resample
    ( resampleWithExtremities
    , MinMax, countSamples, minSample, maxSample, mkMinMax', mmSpan, mmEmpty
    , resampleMinMaxLinear
    , resampleMinMaxLogarithmic
    ) where

import           Imj.Prelude

import           Data.List(length, replicate, take, splitAt)

import           Imj.Math.Root.Newton

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
     let res
          | m' == 0   = replicateElements r input
          | otherwise = let overRepIdx = getOverRepIdx $ assert (m' > 0) 0
                        in go 0 overRepIdx 0 input
     in assert (verifyResample res) res
 where
  input = take n input'

  -- \( r = floor(m/n) \) : every sample will be replicated
  -- \( r \) times, or \( r + 1 \) times if distance to next overrepresentation == 0
  r = quot m n

  -- over-represented samples count
  m' = m - (r * n)

  go _ _ _ [] = []
  go curIdx overRepIdx s (v:vs)
  -- uncomment the following line to debug cases where the assert on the line after would fail:
  --        | overIdx < curIdx = error ("\noverIdx " ++ show overIdx ++ "\ncurIdx  " ++ show curIdx ++ "\nm' " ++ show m' ++ "\nn " ++ show n ++ "\ns " ++ show s)
    | assert (overRepIdx >= curIdx) overRepIdx == curIdx =
        let nextS = s + 1
        in replicate (r+1) v ++ go (curIdx + 1) (getOverRepIdx nextS) nextS vs
    | otherwise =
           replicate r     v ++ go (curIdx + 1) overRepIdx            s     vs

  -- Returns maxBound when there is no over-representation
  getOverRepIdx s
    | m' >  1 = floor( 0.5 + (fromIntegral ((n - 1) * s) :: Float) / fromIntegral (m'-1))
    | m' == 1 = if s == 0
                  then
                    quot n 2
                  else
                    maxBound
    | otherwise = assert (m' == 0) maxBound

  verifyResample resampled
    | m == length resampled = True
    | otherwise = error $ "\ninput    " ++ show (length input) ++
                          "\nnSamples " ++ show m ++
                          "\nactual   " ++ show (length resampled)

-- | Represents the range of a signal in a given duration.
data MinMax a = MinMax {
    minSample :: !a
  , maxSample :: !a
  , countSamples :: {-# UNPACK #-} !Int
} deriving(Show, Eq)
instance Functor MinMax where
  fmap f (MinMax a b n) = MinMax (f a) (f b) n

mkMinMax' :: a -> a -> Int -> MinMax a
mkMinMax' a b c = MinMax a b c

{-# INLINABLE mmSpan #-}
mmSpan :: (Num a) => MinMax a -> a
mmSpan (MinMax a b _) = b - a

{-# INLINABLE mmEmpty #-}
mmEmpty :: Num a => MinMax a
mmEmpty = MinMax 1 (-1) 0

{-# INLINABLE mkMinMax #-}
mkMinMax :: (Ord a, Num a)
         => [a] -> MinMax a
mkMinMax [] = mmEmpty
mkMinMax (e:rest) = go rest $ MinMax e e 1
 where
  go [] x = x
  go (v:vs) c@(MinMax l h n)
   | v < l = go vs $ MinMax v h $ n + 1
   | v > h = go vs $ MinMax l v $ n + 1
   | otherwise = go vs c


{-# INLINABLE resampleMinMaxLinear #-}
resampleMinMaxLinear :: (Ord a, Num a)
                     => [a]
                     -- ^ Input
                     -> Int
                     -- ^ \( n \) : input length. If input is a finite list,
                     -- it is expected that \( 0 <= n <= \) @length input@
                     -> Int
                     -- ^ \( m \) : output length
                     -> [MinMax a]
                     -- ^ Output
resampleMinMaxLinear i inLen outLen
  | outLen <= 0 = []
  | otherwise = map mkMinMax $ mkEvenlySpreadGroups outLen $ take inLen i


-- | This function finds the multiplicative factor @x@ such that for an output of length @m@,
--
-- * the first output value is computed from the first x^0 input samples,
-- * the 2nd from the next x^1 input samples,
-- * the 3rd from the next x^2 input samples
-- * ...
-- * the 'm'-th from the next x^(w-1) input samples
--
-- And such that exactly @n@ samples are used in the input.
{-# INLINABLE resampleMinMaxLogarithmic #-}
resampleMinMaxLogarithmic :: (Ord a, Num a)
                          => [a]
                          -- ^ Input
                          -> Int
                          -- ^ \( n \) : input length. If input is a finite list,
                          -- it is expected that \( 0 <= n <= \) @length input@
                          -> Int
                          -- ^ \( m \) : output length
                          -> [MinMax a]
                          -- ^ Output
resampleMinMaxLogarithmic i n m
  | m <= 0 || n <= 0 = []
        -- Given the specification of the function, we need to find x >= 1 such that :
        --   n = x^0 + x^1 + .... + x^(m-1)
        --
        -- when x /= 1, this is equivalent to : n = (1-x^m)/(1-x)
        --
        -- we rule out the case where 1 is a root here, so that we can assume that 1 is not a root
        -- in the rest of the function: 1 is a solution iff n = m:
  | m >= n = map (mkMinMax . (:[])) $ take n i -- we also handle m > n here
        -- we handle m = 1 and n == 1 here so that we can assume m >= 2 and n >= 2 in the rest of the function:
  | m == 1 || n == 1 = [mkMinMax $ take n i]
  | otherwise =
      reverse $ go (take n i) 0 0 []
     where
      -- Assuming m >= 2 and n >= 2, we want x /= 1 such that:
      --   n = (1-x^m)/(1-x)
      --   n * (1-x) = 1-x^m
      --   x^m - n*x + n-1 = 0
      --
      -- @f(y) = x^m - n*x + n - 1@ has exactly one root > 1 because:
      --
      -- The derivative of the curve is:
      --  m * x^(m-1) - n
      -- Hence the derivative at 1 is:
      --  m-n
      -- And since this function tends to infinity when x tends to infinity,
      -- if m < n, the derivative at 1 is strictly negative, hence since the function is continuous,
      -- it will cross zero at least once for x > 1. And since the derivative is strictly increasing for
      -- x > 0, we can deduce that there is exactly one root > 1.
      --
      -- Now we try to find lower and upper bounds for this root > 1 :
      --
      -- A lower bound for this root is the smallest x > 1 such that m * x^(m-1) - n = 0
      -- => lowerBound = (n/m)^(1/(m-1))
      lowerBound = (fromIntegral n / fromIntegral m)**(1/(fromIntegral m - 1))
      -- Upperbounds must verify:
      --  x^m > n*x - n + 1
      --  => x^m > (n-1)*x
      --  => x^(m-1) > (n-1)
      --  => upperBound = (n-1)^(1/(m-1))
      upperBound = (fromIntegral n-1)**(1/(fromIntegral m - 1))

      -- m >= 2, hence n/m <= n/2
      -- n >= 2, hence n/2 <= n-1
      -- hence n/m <= n-1, and lowerBound < upperBound, so we can apply the Newton root-finding method:
      mayRoot = findRoot
        (\x -> x^m - fromIntegral n*(x-1) - 1)
        (\x -> fromIntegral m * x^(m-1) - fromIntegral n)
        lowerBound
        upperBound
      (root, _) = fromMaybe (error "logic") mayRoot

--      go :: [a] -> Int -> Int -> [MinMax a] -> [MinMax a]
      go [] _ _ o = o
      go remainingInput inputUsedSofar expo o
        | expo == m-1 = nextO remainingInput
        | otherwise = go rest inputUsedNext (expo+1) $ nextO scope
         where
          inputUsedNext
            | expo == 0 = 1
            | expo == m-1 = n
            | otherwise = round $ (1-root^(expo+1))/(1-root)
          countInputUsedThisIteration = inputUsedNext - inputUsedSofar
          (scope,rest) = splitAt countInputUsedThisIteration remainingInput
          nextO x
            | null x = o
            | otherwise = mkMinMax x:o
