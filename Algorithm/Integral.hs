module Algorithm.Integral (
    trap_integral,
    trap_integral_step
    ) where

import Data.List

import ExpressionParser.ExpressionParser

type Step = Double
type Square = Double

trap_integral_step :: Function -> Double -> Double -> Int -> Square
trap_integral_step _ a b _ | a >= b = 0.0
trap_integral_step _ _ _ 0 = 0.0
trap_integral_step func a b n =
    s + trap_integral_step func (a + h) b (n - 1)
    where
        s = ((f' + f) / 2) * h
        f' = evaluate_func func a
        f = evaluate_func func (a + h)
        h = (b - a) / (fromIntegral n)

trap_integral :: Function -> Double -> Double -> Step -> Square
trap_integral f a b h =
    sum * h' + (f' a + f' b) * h' / 2
    where
        sum = foldl' (+) 0 fi
        fi  = map f' [a + (fromIntegral i) * h' | i <- [1..(n - 1)]]
        h'  = (b - a) / (fromIntegral n)
        n   = ceiling $ (b - a) / h
        f'  = evaluate_func f

simpson_integral :: Function -> Double -> Double -> Step -> Square
simpson_integral f a b h =
    sum * h' * 2 / 3 + (f' a + f' b) * h' / 3
    where
        sum = foldl' (+) 0 fi
        fi  = (map (\ x -> 2 * f' x)
                    [a + (fromIntegral i) * h' | i <- [1, 3 .. (2 * n - 1)]]) ++
              (map f' [a + (fromIntegral i) * h' | i <- [2, 4 .. (2 * n - 2)]])
        h' = (b - a) / (2 * fromIntegral n)
        n  = ceiling ((b - a) / (2 * h))
        f'  = evaluate_func f
