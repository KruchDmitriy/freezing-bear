import Data.Char
import Data.Maybe

type Prior = Int
data UnaryOperator  = Sin Prior | Cos Prior | Tg Prior | Lg Prior | Exp Prior
    deriving (Show, Eq)
data BinaryOperator = Plus Prior | Minus Prior | Mult Prior | Div Prior | Pow Prior
    deriving (Show, Eq)

take_prior_un :: UnaryOperator -> Int
take_prior_un (Sin prior) = prior
take_prior_un (Cos prior) = prior
take_prior_un (Tg  prior) = prior
take_prior_un (Lg  prior) = prior
take_prior_un (Exp prior) = prior

instance Ord UnaryOperator where
    x <= y = (take_prior_un x) <= (take_prior_un y)

take_prior_bin :: BinaryOperator -> Int
take_prior_bin (Plus  prior) = prior
take_prior_bin (Minus prior) = prior
take_prior_bin (Mult  prior) = prior
take_prior_bin (Div   prior) = prior
take_prior_bin (Pow   prior) = prior

instance Ord BinaryOperator where
    x <= y = (take_prior_bin x) <= (take_prior_bin y)

create_un_op :: String -> Maybe UnaryOperator
create_un_op str
    | (str == "sin") = Just (Sin 3)
    | (str == "cos") = Just (Cos 3)
    | (str == "tg")  = Just (Tg  3)
    | (str == "lg")  = Just (Lg  3)
    | (str == "exp") = Just (Exp 3)
    | otherwise      = Nothing

create_bin_op :: Char -> Maybe BinaryOperator
create_bin_op c
    | (c == '+') = Just (Plus  1)
    | (c == '-') = Just (Minus 1)
    | (c == '*') = Just (Mult  2)
    | (c == '/') = Just (Div   2)
    | (c == '^') = Just (Pow   3)
    | otherwise  = Nothing

inc_prior_un :: UnaryOperator -> Int -> UnaryOperator
inc_prior_un (Sin prior) delta = Sin (prior + delta)
inc_prior_un (Cos prior) delta = Cos (prior + delta)
inc_prior_un (Tg  prior) delta = Tg  (prior + delta)
inc_prior_un (Lg  prior) delta = Lg  (prior + delta)
inc_prior_un (Exp prior) delta = Exp (prior + delta)

inc_prior_bin :: BinaryOperator -> Int -> BinaryOperator
inc_prior_bin (Plus  prior) delta = Plus  (prior + delta)
inc_prior_bin (Minus prior) delta = Minus (prior + delta)
inc_prior_bin (Mult  prior) delta = Mult  (prior + delta)
inc_prior_bin (Div   prior) delta = Div   (prior + delta)
inc_prior_bin (Pow   prior) delta = Pow   (prior + delta)

eval_un :: UnaryOperator -> Double -> Double
eval_un (Sin _) value = sin(value)
eval_un (Cos _) value = cos(value)
eval_un (Tg  _) value = tan(value)
eval_un (Lg  _) value = log(value)
eval_un (Exp _) value = exp(value)

eval_bin :: BinaryOperator -> Double -> Double -> Double
eval_bin (Plus  _) val1 val2 = val1 + val2
eval_bin (Minus _) val1 val2 = val1 - val2
eval_bin (Mult  _) val1 val2 = val1 * val2
eval_bin (Div   _) val1 val2 = val1 / val2
eval_bin (Pow   _) val1 val2 = val1 ** val2

data Token = UnOp UnaryOperator | BinOp BinaryOperator |
             LeftBr | RightBr |
             X | Number Double
             deriving (Show, Eq)

instance Ord Token where
    (UnOp u) <= (BinOp b) = ((take_prior_un u) < (take_prior_bin b))
    (BinOp b1) <= (BinOp b2) = b1 <= b2
    (UnOp u1) <= (UnOp u2) = u1 <= u2

takeWhile_n_tail :: [a] -> (a -> Bool) -> ([a], [a])
takeWhile_n_tail [] _ = ([], [])
takeWhile_n_tail (x:xs) p
    | p x         = (x : fst xn, snd xn)
    | otherwise   = ([], (x:xs))
    where xn = takeWhile_n_tail xs p

tokenizer :: String -> [Token]
tokenizer "" = []
tokenizer (c:cs)
    | c == ' '          = tokenizer cs
    | isDigit c         =
        Number (read (fst num_n_tail) :: Double) : (tokenizer $ snd num_n_tail)
    | isLetter c && (c /= 'x') =
        if (un_op /= Nothing) then UnOp (fromJust un_op) : (tokenizer $ snd func_n_tail)
        else []
    | bin_op /= Nothing = BinOp (fromJust bin_op) : tokenizer cs
    | c == 'x'          = X : tokenizer cs
    | c == '('          = LeftBr : tokenizer cs
    | c == ')'          = RightBr : tokenizer cs
    | otherwise         = []
    where num_n_tail  = takeWhile_n_tail (c:cs) (\x -> isDigit x || x == '.')
          func_n_tail = takeWhile_n_tail (c:cs) isLetter
          un_op = create_un_op (fst func_n_tail)
          bin_op = create_bin_op c

check_prior :: [Token] -> Int -> [Token]
check_prior [] _ = []
check_prior (t:ts) prior =
    case t of
        LeftBr    -> check_prior ts (prior + 3)
        RightBr   -> check_prior ts (prior - 3)
        UnOp un   -> (UnOp  (inc_prior_un un prior)) : (check_prior ts prior)
        BinOp bin -> (BinOp (inc_prior_bin bin prior)) : (check_prior ts prior)
        _         -> t : (check_prior ts prior)

data Term = BiTerm BinaryOperator Term Term |
            UnTerm UnaryOperator  Term      |
            VarX | VarNumber Double |
            Null
            deriving (Show, Eq)

type LexTree = Term

is_bin_op :: Token -> Bool
is_bin_op (BinOp b) = True
is_bin_op _ = False

is_un_op :: Token -> Bool
is_un_op (UnOp b) = True
is_un_op _ = False

type UnOp_n_Arg  = Maybe (Token, [Token])
type BinOp_n_Arg = Maybe (Token, [Token], [Token])

minimum' :: Ord a => [a] -> Maybe a
minimum' [] = Nothing
minimum' list = Just (minimum list)

find_weak_un_op :: [Token] -> UnOp_n_Arg
find_weak_un_op []     = Nothing
find_weak_un_op tokens =
    case token of
        Just tok -> Just (tok, tail $ snd token_n_tail)
            where token_n_tail = takeWhile_n_tail tokens (\x -> x /= tok)
        _        -> Nothing
        where
            token = minimum' (filter is_un_op tokens)

find_weak_bin_op :: [Token] -> BinOp_n_Arg
find_weak_bin_op []     = Nothing
find_weak_bin_op tokens =
    case token of
        Just tok -> Just (tok, fst token_n_tail, tail $ snd token_n_tail)
            where token_n_tail = takeWhile_n_tail tokens (\x -> x /= tok)
        _        -> Nothing
        where
            token = minimum' (filter is_bin_op tokens)

from_token_to_bin :: Token -> BinaryOperator
from_token_to_bin (BinOp b) = b

from_token_to_un :: Token -> UnaryOperator
from_token_to_un (UnOp u) = u

find_weak_op :: [Token] -> Either UnOp_n_Arg BinOp_n_Arg
find_weak_op tokens =
    if (weak_un /= Nothing && weak_bin /= Nothing) then
        if ((fst $ fromJust weak_un) < (fst' $ fromJust weak_bin)) then Left weak_un
        else Right weak_bin
    else if (weak_un /= Nothing) then Left weak_un
         else if (weak_bin /= Nothing) then Right weak_bin
              else Left Nothing
    where
        weak_un  = find_weak_un_op tokens
        weak_bin = find_weak_bin_op tokens
        fst' = \ (x1, _, _) -> x1

-- If we have at least one Null in LexTree, LexTree is incorrect
terminator :: [Token] -> LexTree
terminator [X]        = VarX
terminator [Number a] = VarNumber a
terminator tokens =
    case weak_op_ht of
        Left  (Just (UnOp u, tok))          -> UnTerm u (terminator tok)
        Right (Just (BinOp b, tok1, tok2))  -> BiTerm b (terminator tok1) (terminator tok2)
        Left Nothing                        -> Null
    where
        weak_op_ht = find_weak_op tokens


evaluator :: LexTree -> Double -> Double
evaluator VarX value                        = value
evaluator (VarNumber number) _              = number
evaluator (UnTerm un_op term) value         = eval_un  un_op (evaluator term value)
evaluator (BiTerm bi_op term1 term2) value  = eval_bin bi_op (evaluator term1 value) (evaluator term2 value)
