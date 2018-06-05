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
-- This function assumes that the function passed as argument has /exactly/
-- one root within the given interval, and is well-behaved.
--
-- We will use different strategies wether f(lower_bound) and f(upper_bound) have different signs
-- or not: if they have different signs, we use a mix of bisect and newton steps,
-- starting from the middle of the interval. If they have the same sign, we use
-- newton steps exclusivley, starting from both ends of the interval.
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
    -- low and high values have different signs. Under the assumption that the function has
    -- /exactly/ one root within the interval, we could find this root by bisecting.
    -- To go faster, we use newton steps, and use bisection only when a newton step goes outside
    -- the current interval.
    -- (TODO when numerical errors become significant w.r.t the function values,
    -- maybe all newton step will fall outside the interval and we should bisect only
    -- to optimize speed.)
      let firstInterval = RootInterval low vLow high vHigh
      in go (intervalCenter firstInterval) firstInterval
  | otherwise =
    -- low and high values have the same sign.
    --
    -- We can't use the bissect strategy, because
    -- if the middle point has the same sign as extremities, we don't know which interval
    -- to discard, and if the middle point has a different sign than extremities, it means there are at least
    -- two roots, which breaks the assumption that the function has /exactly/ one root within the interval.
    --
    -- Hence, we do newton steps exclusively, from both interval ends, and hope that at least
    -- one of them will be successful:
    -- (TODO continue the search starting from elsewhere within the interval until we find a solution)
      case catMaybes
        [ newtonRoot low Nothing
        , newtonRoot high Nothing
        ] of
        [] -> Nothing
        [one] -> Just one
        [exact@(_,Exact),_] -> Just exact
        [_,exact@(_,Exact)] -> Just exact
        [(v1, StoppedProgressing), (v2, StoppedProgressing)] -> Just (0.5 * (v1 + v2), StoppedProgressing)
        _:_:_ -> error "logic"

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

   where

    vX = f x
    dX = df x

    newton = x - vX / dX

    strictlyInInterval = x > l && x < h

    reduceIntervalStrictly
      | vX * vL < 0 = RootInterval l vL x vX
      | vX * vH < 0 = RootInterval x vX h vH
      | otherwise = error "logic"

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
      | vFrom < vB = newtonRoot newton $ Just (from, vFrom, 1::Int) -- we have a new best.
      | n > 100 = Just (b, StoppedProgressing) -- the best didn't change in 100 iterations, return it.
      | otherwise = newtonRoot newton $ Just (b, vB, n+1)
