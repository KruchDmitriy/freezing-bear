import Data.Char
import Data.Maybe

data UnaryOperator  = UnMinus | Sin | Cos | Tg | Ctg | Lg | Exp
    deriving (Show, Eq)
data BinaryOperator = Plus | Minus | Mult | Div | Pow
    deriving (Show, Eq)

is_un_op :: String -> Maybe UnaryOperator
is_un_op str
    | (str == "-")   = Just UnMinus
    | (str == "sin") = Just Sin
    | (str == "cos") = Just Cos
    | (str == "tg")  = Just Tg
    | (str == "ctg") = Just Ctg
    | (str == "lg")  = Just Lg
    | (str == "exp") = Just Exp
    | otherwise      = Nothing

is_bin_op :: Char -> Maybe BinaryOperator
is_bin_op c
    | (c == '+') = Just Plus
    | (c == '-') = Just Minus
    | (c == '*') = Just Mult
    | (c == '/') = Just Div
    | (c == '^') = Just Pow
    | otherwise  = Nothing

data Token = UnOp UnaryOperator | BinOp BinaryOperator |
             LeftBr | RightBr |
             X | Number Double
             deriving (Show, Eq)

takeWhile_n_tail :: String -> (Char -> Bool) -> (String, String)
takeWhile_n_tail "" _ = ("", "")
takeWhile_n_tail (c:cs) p
    | p c         = (c : fst cn, snd cn)
    | otherwise   = ("", (c:cs))
    where cn = takeWhile_n_tail cs p

tokenizer :: String -> [Token]
tokenizer "" = []
tokenizer (c:cs)
    | isDigit c         =
        Number (read (fst num_n_tail) :: Double) : (tokenizer $ snd num_n_tail)
    | isLetter c =
        if (un_op /= Nothing) then UnOp (fromJust un_op) : (tokenizer $ snd func_n_tail)
        else []
    | bin_op /= Nothing = BinOp (fromJust bin_op) : tokenizer cs
    | c == 'x'          = X : tokenizer cs
    | c == '('          = LeftBr : tokenizer cs
    | c == ')'          = RightBr : tokenizer cs
    | otherwise         = []
    where num_n_tail  = takeWhile_n_tail (c:cs) (\x -> isDigit x || x == '.')
          func_n_tail = takeWhile_n_tail (c:cs) isLetter
          un_op = is_un_op (fst func_n_tail)
          bin_op = is_bin_op c
