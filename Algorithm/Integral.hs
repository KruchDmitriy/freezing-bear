module Algorithm.Integral (
    trap_integral_count,
    trap_integral_step,
    simpson_integral
    ) where

import Data.List

import ExpressionParser.ExpressionParser

type Step = Double
type Area = Double

trap_integral_count :: Function -> Double -> Double -> Int -> Area
trap_integral_count _ a b _ | a >= b = 0.0
trap_integral_count _ _ _ 0 = 0.0
trap_integral_count func a b n =
    s + trap_integral_count func (a + h) b (n - 1)
    where
        s = ((f' + f) / 2) * h
        f' = evaluate_func func a
        f = evaluate_func func (a + h)
        h = (b - a) / (fromIntegral n)

trap_integral_step :: Function -> Double -> Double -> Step -> Area
trap_integral_step f a b h =
    sum * h' + (f' a + f' b) * h' / 2
    where
        sum = foldl' (+) 0 fi
        fi  = map f' [a + (fromIntegral i) * h' | i <- [1..(n - 1)]]
        h'  = (b - a) / (fromIntegral n)
        n   = ceiling $ (b - a) / h
        f'  = evaluate_func f

simpson_integral :: Function -> Double -> Double -> Step -> Area
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
