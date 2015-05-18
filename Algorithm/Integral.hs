module Algorithm.Integral (
    trapeze_integrate
    ) where

import Data.List

import ExpressionParser.ExpressionParser

--trapeze_integrate_by_stepcount :: Function -> Double -> Double -> Int -> Double
--trapeze_integrate_by_stepcount _ a b _ | a >= b = 0.0
--trapeze_integrate_by_stepcount _ _ _ 0 = 0.0
--trapeze_integrate_by_stepcount func a b n =
--    s + trapeze_integrate func (a + h) b (n - 1)
--    where
--        s = ((f' + f) / 2) * h
--        f' = evaluate_func func a
--        f = evaluate_func func (a + h)
--        h = (b - a) / (fromIntegral n)

type Step = Double
trapeze_integrate :: Function -> Double -> Double -> Step -> Double
trapeze_integrate f a b h =
    sum * h + (f' a + f' b) * h / 2
    where
        sum = foldl' (+) 0 fi
        fi = map f' [a + (fromIntegral i) * h | i <- [1..(n - 1)]]
        n = ceiling $ (b - a) / h
        f' = evaluate_func f