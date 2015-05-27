module Main where

import ExpressionParser.ExpressionParser
import Algorithm.Integral
import Algorithm.Interpolation
import Plot.Plot

import Data.Text
import Data.String

help :: IO ()
help = do
    putStrLn "Input:"
    putStrLn "1. Integrate by trapezy method (steps count)"
    putStrLn "2. Integrate by trapezy method (step)"
    putStrLn "3. Integrate by Simpson's method"
    putStrLn "4. Interpolate by Lagrange method"

type TrapezyIntegralParams = (Double, Double, Int)
type IntegralParams = (Double, Double, Double)

request_range_count :: IO ()
request_range_count = putStrLn "Enter a, b range and steps count by whitespace:"

request_range_step :: IO()
request_range_step = putStrLn "Enter a, b range and the step by whitespace:"

request_func :: IO ()
request_func = putStrLn "Enter the function:"

parse_range_count :: Text -> TrapezyIntegralParams
parse_range_count s = params
    where
        str_params = split (==' ') s
        params = (read $ unpack (str_params!!0) :: Double,
                read $ unpack (str_params!!1) :: Double,
                read $ unpack (str_params!!2) :: Int)

parse_range_step :: Text -> IntegralParams
parse_range_step s = params
    where
        str_params = split (==' ') s
        params = (read $ unpack (str_params!!0) :: Double,
                read $ unpack (str_params!!1) :: Double,
                read $ unpack (str_params!!2) :: Double)

fst' = \ (x, y, z) -> x
snd' = \ (x, y, z) -> y
thd' = \ (x, y, z) -> z

main = do
    help
    action <- getLine
    request_func
    str_func <- getLine
    case action of
        "1" -> do
            request_range_count
            str_params <- getLine
            let func = create_func str_func
                params = parse_range_count $ fromString str_params
                a = fst' params
                b = snd' params
                count = thd' params
            putStrLn $ "Result: " ++ (show $ trap_integral_count func a b count)
        "2" -> do
            request_range_step
            str_params <- getLine
            let func = create_func str_func
                params = parse_range_step $ fromString str_params
                a = fst' params
                b = snd' params
                step = thd' params
            putStrLn $ "Result: " ++ (show $ trap_integral_step func a b step)
        "3" -> do
            request_range_step
            str_params <- getLine
            let func = create_func str_func
                params = parse_range_step $ fromString str_params
                a = fst' params
                b = snd' params
                step = thd' params
            putStrLn $ "Result: " ++ (show $ simpson_integral func a b step)
        _   -> putStrLn "Incorrect params"
        