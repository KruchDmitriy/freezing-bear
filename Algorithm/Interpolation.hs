import ExpressionParser.Operator.BinaryOperator
import ExpressionParser.ExpressionParser

type Point = (Double, Double)

get_diff :: Double -> Function
get_diff xi = create_func $ "(x - " ++ (show xi) ++ ")"



--interpolate_lagrange :: [Point] -> Function
--interpolate_lagrange [] = create_func "0"
--interpolate_lagrange points = 