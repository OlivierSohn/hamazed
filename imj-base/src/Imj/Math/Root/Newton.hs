{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Math.Root.Newton
  ( findRoot
  , RootQuality(..)
  ) where

import Imj.Prelude

data RootQuality =
    Exact
    -- ^ f(x) = 0
  | StoppedProgressing
    -- Due to numerical approximations, we couldn't get to the root but got close.
  deriving(Show, Eq)

data RootInterval = RootInterval {
  _lowX, _lowY, _highX, _highY :: {-# UNPACK #-} !Double
} deriving(Show)


intervalCenter :: RootInterval -> Double
intervalCenter (RootInterval a _ b _) = 0.5 * (a + b)

-- | Uses <https://en.wikipedia.org/wiki/Newton%27s_method the Newton method>
--  to find the root of a function, within a given interval.
--
-- We expect that the function has /exactly/ one root within this interval,
-- and is well-behaved.
--
-- We use different strategies wether f(lower_bound) and f(upper_bound) have different signs
-- or not.
--
-- <https://math.stackexchange.com/questions/1892104/easy-conditions-that-guarantee-the-convergence-of-newtons-method This discussion>
-- gives sufficient conditions so that a root will be found.
findRoot :: (Double -> Double)
         -- ^ f, the function whose root we want to find.
         -> (Double -> Double)
         -- ^ First derivative of f.
         -> Double
         -- ^ Lower bound of the root.
         -> Double
         -- ^ Upper bound of the root
         -> Maybe (Double, RootQuality)
         -- ^ The result, or 'Nothing' if we didn't converge
findRoot f df low high
  | vLow == 0 = Just (low, Exact)
  | vHigh == 0 = Just (high, Exact)
  | vLow * vHigh < 0 =
      let firstInterval = RootInterval low vLow high vHigh
      in go (intervalCenter firstInterval) firstInterval
  | otherwise =  -- start from both extremities, do newton iterations while vX decreases, then return the middle (or the exact root if found)
      let bestLeftRoot = newtonRoot low Nothing
          bestRightRoot = newtonRoot high Nothing
          newtonRoot from best
            | from < low || from > high = Nothing
            | vFrom == 0 = Just (from, Exact)
            | otherwise = maybe
                (newtonRoot newton $ Just (from,vFrom,1))
                takeBest
                best
           where
            vFrom = f from
            dFrom = df from
            newton = from - vFrom / dFrom

            takeBest (b, vB, n)
              | b == from = Just (b, StoppedProgressing) -- we are in a cycle
              | vFrom < vB = newtonRoot newton $ Just (from, vFrom, 1::Int) -- we have a new best.
              | n > 100 = Just (b, StoppedProgressing) -- the best didn't change in 100 iterations, return it.
              | otherwise = newtonRoot newton $ Just (b, vB, n+1)

      in case catMaybes [bestLeftRoot, bestRightRoot] of
        [] -> Nothing
        [one] -> Just one
        [exact@(_,Exact),_] -> Just exact
        [_,exact@(_,Exact)] -> Just exact
        [(v1, StoppedProgressing), (v2, StoppedProgressing)] -> Just (0.5 * (v1 + v2), StoppedProgressing)
        _:_:_ -> error "logic"
        -- TODO continue the search starting from elsewhere within the interval until we find a solution

 where

  vLow = f low
  vHigh = f high

  go x i@(RootInterval l vL h vH)
    | vX == 0 = Just (x, Exact)
    | strictlyInInterval =
        let ri@(RootInterval rl _ rh _) = reduceIntervalStrictly
        in if rl == rh
          then
            Just (rl, Exact)
          else
            go newton ri
    | otherwise =
        let c = intervalCenter i
        in if c <= l || c >= h
          then
            Just (c, StoppedProgressing)
          else
            go c i
    -- Note that at some point, when numerical errors become relevant, bisecting may converge faster than newton.

   where

    vX = f x
    dX = df x

    newton = x - vX / dX

    strictlyInInterval = x > l && x < h

    reduceIntervalStrictly
      | vX * vL < 0 = RootInterval l vL x vX
      | vX * vH < 0 = RootInterval x vX h vH
      | otherwise = error "logic"
