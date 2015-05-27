module Algorithm.Interpolation (
    interpolate_lagrange,
    Point
) where

import ExpressionParser.Operator.BinaryOperator
import ExpressionParser.ExpressionParser

import Data.List

type Point = (Double, Double)

get_diff_nom :: Double -> Function
get_diff_nom xi = create_func $ "(x - " ++ (change_negative xi) ++ ")"

get_diff_denom :: Double -> Double -> Double
get_diff_denom xi x = xi - x

transform_points :: [Double] -> Int -> Int -> [Double]
transform_points [] _ _ = []
transform_points (d:ds) i n
            | (i == n) = transform_points ds (i + 1) n
            | otherwise = d : transform_points ds (i + 1) n

tr_list_points :: [Double] -> Int -> Int -> [[Double]]
tr_list_points [] _ _ = [[]]
tr_list_points points i len
        | (i /= len) = transform_points points 0 i :
            (tr_list_points points (i + 1) len)
        | otherwise = []

product_diffs_nom :: [Double] -> Function
product_diffs_nom points = foldl' (*) (create_func "1") (map get_diff_nom points)

product_diffs_denom :: Double -> [Double] -> Double
product_diffs_denom xi points = foldl' (*) 1.0 (map (\ ps -> get_diff_denom xi ps) points)

create_denoms :: [[Double]] -> [Double] -> [Function]
create_denoms pss xs =
    map (create_func . change_negative) zipped
    where
        zipped = zipWith (\ x y -> product_diffs_denom y x) pss xs

change_negative :: Double -> String
change_negative = 
    (\ x -> if x > 0 then show x else "0" ++ (show x))

interpolate_lagrange :: [Point] -> Function
interpolate_lagrange [] = create_func "0"
interpolate_lagrange points = sum s
    where
        noms = map product_diffs_nom tr_points
        denoms = create_denoms tr_points fst_s
        tr_points = tr_list_points fst_s 0 (length fst_s)
        fst_s = map fst points
        snd_s = map (create_func . change_negative . snd) points
        s = zipWith3 (\ x y z -> x / y * z) noms denoms snd_s