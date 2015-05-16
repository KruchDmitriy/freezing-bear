module Algorithm.Integral (
    trapeze_integrate
    ) where

import ExpressionParser.ExpressionParser

trapeze_integrate :: Function -> Double -> Double -> Int -> Double
trapeze_integrate _ a b _ | a >= b = 0.0
trapeze_integrate _ _ _ 0 = 0.0
trapeze_integrate func a b n =
    s + trapeze_integrate func (a + h) b (n - 1)
    where
        s = ((f' + f) / 2) * h
        f' = evaluate_func func a
        f = evaluate_func func (a + h)
        h = (b - a) / (fromIntegral n)