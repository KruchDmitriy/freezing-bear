module ExpressionParser.Operator.UnaryOperator
(
    UnaryOperator,
    take_prior_un,
    create_un_op,
    inc_prior_un,
    eval_un
) where

type Prior = Int
data UnaryOperator  =
        Sin Prior |
        Cos Prior |
        Tg  Prior |
        Lg  Prior |
        Exp Prior
    deriving (Eq)

instance Show UnaryOperator where
    show (Sin _) = "sin"
    show (Cos _) = "cos"
    show (Tg  _) = "tg"
    show (Lg  _) = "lg"
    show (Exp _) = "exp"

take_prior_un :: UnaryOperator -> Prior
take_prior_un (Sin prior) = prior
take_prior_un (Cos prior) = prior
take_prior_un (Tg  prior) = prior
take_prior_un (Lg  prior) = prior
take_prior_un (Exp prior) = prior

instance Ord UnaryOperator where
    x <= y = (take_prior_un x) <= (take_prior_un y)

create_un_op :: String -> Maybe UnaryOperator
create_un_op str
    | (str == "sin") = Just (Sin 5)
    | (str == "cos") = Just (Cos 5)
    | (str == "tg")  = Just (Tg  5)
    | (str == "lg")  = Just (Lg  5)
    | (str == "exp") = Just (Exp 5)
    | otherwise      = Nothing

inc_prior_un :: UnaryOperator -> Int -> UnaryOperator
inc_prior_un (Sin prior) delta = Sin (prior + delta)
inc_prior_un (Cos prior) delta = Cos (prior + delta)
inc_prior_un (Tg  prior) delta = Tg  (prior + delta)
inc_prior_un (Lg  prior) delta = Lg  (prior + delta)
inc_prior_un (Exp prior) delta = Exp (prior + delta)

eval_un :: UnaryOperator -> Double -> Double
eval_un (Sin _) value = sin(value)
eval_un (Cos _) value = cos(value)
eval_un (Tg  _) value = tan(value)
eval_un (Lg  _) value = log(value)
eval_un (Exp _) value = exp(value)
