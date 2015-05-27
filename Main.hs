module Main where

import ExpressionParser.ExpressionParser
import Algorithm.Integral
import Algorithm.Interpolation
import Plot.Plot

import Data.Text
import Data.String
import Graphics.Rendering.OpenGL

help :: IO ()
help = do
    putStrLn "Input:"
    putStrLn "1. Integrate by trapezy method (steps count)"
    putStrLn "2. Integrate by trapezy method (step)"
    putStrLn "3. Integrate by Simpson's method"
    putStrLn "4. Interpolate by Lagrange method"

type TrapezyIntegralParams = (Double, Double, Int)
type FuncParams = (Double, Double, Double)

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

parse_range_step :: Text -> FuncParams
parse_range_step s = params
    where
        str_params = split (==' ') s
        params = (read $ unpack (str_params!!0) :: Double,
                read $ unpack (str_params!!1) :: Double,
                read $ unpack (str_params!!2) :: Double)

gen_points_by_func :: Function -> FuncParams -> [Point]
gen_points_by_func func (a, b, step) = Prelude.zip xs ys
    where
        xs = [a, a + step .. b]
        ys = Prelude.map (evaluate_func func) xs

fst' = \ (x, y, z) -> x
snd' = \ (x, y, z) -> y
thd' = \ (x, y, z) -> z

-- TODO: Wrap to functions
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
        "4" -> do
            request_range_step
            str_params <- getLine
            let func = create_func str_func
                params = parse_range_step $ fromString str_params
                a = fst' params
                b = snd' params
                step = thd' params
                points = gen_points_by_func func (a, b, step)
                polynom = interpolate_lagrange points
                descr_polynom = DescriptorFunc polynom a b (step / 100.0) $ Color4 0 0 1 1
                descr_grth = DescriptorFunc func a b (step / 100.0) $ Color4 0 1 0 1
                queue = add_func (add_func [] descr_polynom) descr_grth
            draw_window queue
        _   -> putStrLn "Incorrect params"
        