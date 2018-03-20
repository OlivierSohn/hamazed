{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Graphics.Math.Ease
      (
      -- * 4th order /inverse/ easing, continuous
      {- | Easing is traditionally seen as a function from /time/ to value.

        Here, it is a function from /value/ to time, hence the use of the term /Inverse/ in the title.
      -}
        invQuartEaseInOut
      -- * From continuous to discrete
      {- |
      Easing in a continuous world is /easy/ (no pun intended), but easing in a
      discrete world is harder : we have to make sure the discretization will
      not break the visual easing effect.

      The 'discreteAdaptor' function does just that, making a continuous easing
      function usable in a discrete context.
      -}
      , discreteAdaptor
      -- * 4th order inverse easing, discrete
      -- | Using 'discreteAdaptor' on 'invQuartEaseInOut' we can make
      -- 'discreteInvQuartEaseInOut' :
      , discreteInvQuartEaseInOut
      -- * Adaptors
      , inOutToIn
      ) where

import           Imj.Prelude

-- cf. this for formatting : https://math.meta.stackexchange.com/questions/5020/mathjax-basic-tutorial-and-quick-reference

{- |
Returns the time \( t \in [\,0,1]\, \)  at which a value \( y \in [\,0,1]\,\) is reached
given a <http://gizma.com/easing/ 4th order ease in-out function> \( quartEaseInOut \):

\[ y = quartEaseInOut(t) =
\begin{cases}
{1 \over 2} *  (2*t)^4,                          & \;\;\;\; \text{if $t < {1 \over 2}$} \\[2ex]
-{1 \over 2} * \left( [ 2*(t-1) ]^4 - 2 \right), & \;\;\;\; \text{if $t > {1 \over 2}$}
\end{cases}
\]

To find the formulas of 'invQuartEaseInOut', we need to invert \( quartEaseInOut \),
i.e. we need to express \(t\) in terms of \(y\):

\[ \text{$quartEaseInOut$ is strictly increasing} \implies
\begin{cases}
t<{1 \over 2} \iff y<{1 \over 2} \\
t>{1 \over 2} \iff y>{1 \over 2}
\end{cases}
\]


\[ \begin{alignedat}{3}
\text{if $y < {1 \over 2} $, given the $quartEaseInOut$ equation for $t < {1 \over 2} $ :}
  &&                            y &= {1 \over 2} * (2*t)^4   &&  \\
  \implies &&  \quad            t &= \left({y \over 2^3}\right)^{1/4} && \quad \forall y < {1 \over 2} \\
\text{if $y > {1 \over 2} $, given the $quartEaseInOut$ equation for $t > {1 \over 2} $ :}
  &&                y &= - {1 \over 2} * \left( [2*(t-1)]^4 - 2 \right) &&  \\
  \implies && \quad t &= 1-\left[{1-y \over 2^3}\right]^{1/4}    && \quad \forall y > {1 \over 2}
\end{alignedat} \]

/Note that there are multiple solutions, we chose the ones that produce results in the \( [\,0,1]\, \) range./

Hence, the formulas for 'invQuartEaseInOut' are :

\[ t = invQuartEaseInOut(y) =
\begin{cases}
\left({y \over 2^3}\right)^{1/4},     & \text{if $y < {1 \over 2}$} \\[2ex]
1-\left[{1-y \over 2^3}\right]^{1/4}, & \text{if $y > {1 \over 2}$}
\end{cases}
\]

 -}
invQuartEaseInOut :: Double
                  -- ^ Value : \( y \)
                  -> Double
                  -- ^ Time : \( t \)
invQuartEaseInOut y =
  if y < 0.5
    then
      (y / 8.0) ** (1.0/4.0)
    else
      1.0 - ((1.0 - y) / 8.0) ** (1.0/4.0)

inOutToIn :: (Double -> Double) -> Double -> Double
inOutToIn f i = 2 * f (i/2)

-- | Adapts continuous inout ease functions to the discrete case.
discreteAdaptor :: (Double -> Double)
                -- ^ Continuous (optionally inverse) ease in/out function
                -> Int
                -- ^ The number of discrete steps
                -> Double
                -- ^ Input value
                -> Double
                -- ^ (optionnaly inverse) Eased value
discreteAdaptor f n v =
  -- We use the center of the intervals instead of the extremities.
  let nIntervals = n
      intervalSize = recip $ fromIntegral nIntervals
      firstValue = intervalSize / 2
      lastValue = 1 - firstValue
      scaledValue = firstValue + v * (lastValue - firstValue)
  in f scaledValue

-- | Returns the time (in range [0 1]) at which a value (in range [0 1]) is reached
-- given a 4th order ease in-out function, and a total number of discrete steps.
discreteInvQuartEaseInOut :: Int
                          -- ^ The number of discrete steps
                          -> Double
                          -- ^ Value
                          -> Double
                          -- ^ Time
discreteInvQuartEaseInOut = discreteAdaptor invQuartEaseInOut
