module Main where

import ExpressionParser.ExpressionParser
import Algorithm.Integral
import Algorithm.Interpolation
import Plot.Plot

help :: IO ()
help = do
    putStrLn "Input:"
    putStrLn "1. Integrate by trapezy method"
    putStrLn "2. Integrate by Simpson's method"
    putStrLn "3. Interpolate by Lagrange method"

request_func :: IO ()
request_func = putStrLn "Enter the function:"

main = do
    help