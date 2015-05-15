module Operator.BinaryOperator
(
    BinaryOperator,
    take_prior_bin,
    create_bin_op,
    inc_prior_bin,
    eval_bin
) where

type Prior = Int
data BinaryOperator =
        Plus  Prior |
        Minus Prior |
        Mult  Prior |
        Div   Prior |
        Pow   Prior
    deriving (Show, Eq)

take_prior_bin :: BinaryOperator -> Prior
take_prior_bin (Plus  prior) = prior
take_prior_bin (Minus prior) = prior
take_prior_bin (Mult  prior) = prior
take_prior_bin (Div   prior) = prior
take_prior_bin (Pow   prior) = prior

instance Ord BinaryOperator where
    x <= y = (take_prior_bin x) <= (take_prior_bin y)

create_bin_op :: Char -> Maybe BinaryOperator
create_bin_op c
    | (c == '+') = Just (Plus  1)
    | (c == '-') = Just (Minus 1)
    | (c == '*') = Just (Mult  2)
    | (c == '/') = Just (Div   2)
    | (c == '^') = Just (Pow   3)
    | otherwise  = Nothing

inc_prior_bin :: BinaryOperator -> Int -> BinaryOperator
inc_prior_bin (Plus  prior) delta = Plus  (prior + delta)
inc_prior_bin (Minus prior) delta = Minus (prior + delta)
inc_prior_bin (Mult  prior) delta = Mult  (prior + delta)
inc_prior_bin (Div   prior) delta = Div   (prior + delta)
inc_prior_bin (Pow   prior) delta = Pow   (prior + delta)

eval_bin :: BinaryOperator -> Double -> Double -> Double
eval_bin (Plus  _) val1 val2 = val1 + val2
eval_bin (Minus _) val1 val2 = val1 - val2
eval_bin (Mult  _) val1 val2 = val1 * val2
eval_bin (Div   _) val1 val2 = val1 / val2
eval_bin (Pow   _) val1 val2 = val1 ** val2
