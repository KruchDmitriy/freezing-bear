module ExpressionParser.ExpressionParser (
    Function,
    create_func,
    evaluate_func
) where

import Data.Char
import Data.Maybe
import Data.List

import ExpressionParser.Operator.UnaryOperator
import ExpressionParser.Operator.BinaryOperator

data Token = UnOp UnaryOperator | BinOp BinaryOperator |
             LeftBr | RightBr |
             X | Number Double
             deriving (Show, Eq)

instance Ord Token where
    (UnOp u) <= (BinOp b) = ((take_prior_un u) < (take_prior_bin b))
    (BinOp b1) <= (BinOp b2) = b1 <= b2
    (UnOp u1) <= (UnOp u2) = u1 <= u2

takeWhile_r_tail :: [a] -> (a -> Bool) -> ([a], [a])
takeWhile_r_tail [] _ = ([], [])
takeWhile_r_tail (x:xs) p
    | p x         = (x : fst xn, snd xn)
    | otherwise   = ([], (x:xs))
    where xn = takeWhile_r_tail xs p

takeWhile_l_tail :: [a] -> (a -> Bool) -> ([a], [a])
takeWhile_l_tail [] _ = ([], [])
takeWhile_l_tail (x:xs) p
    | p x         = (x : fst xn, snd xn)
    | otherwise   = ([x], xs)
    where xn = takeWhile_l_tail xs p


tokenizer :: String -> [Token]
tokenizer "" = []
tokenizer (c:cs)
    | c == ' '          = tokenizer cs
    | isDigit c         =
        Number (read (fst $ num_tail (c:cs)) :: Double) : (tokenizer $ snd $ num_tail (c:cs))
    | isLetter c && (c /= 'x') =
        case un_op of
            (Just (E _)) -> 
                if head cs == '-' then
                    BinOp(fromJust $ create_bin_op '*') : UnOp(fromJust un_op) : 
                        (LeftBr) : (Number 0.0) : BinOp(fromJust $ create_bin_op '-') : Number(read (fst $ num_tail (tail cs)) :: Double) : (RightBr) :
                        (tokenizer $ snd $ num_tail (tail cs))
                else 
                    BinOp(fromJust $ create_bin_op '*') : UnOp(fromJust un_op) : (tokenizer $ snd $ func_tail (c:cs))
            (Just _)   -> UnOp (fromJust un_op) : (tokenizer $ snd $ func_tail (c:cs))
            Nothing    -> []
    | bin_op /= Nothing = BinOp (fromJust bin_op) : tokenizer cs
    | c == 'x'          = X : tokenizer cs
    | c == '('          = LeftBr : tokenizer cs
    | c == ')'          = RightBr : tokenizer cs
    | otherwise         = []
    where num_tail str  = takeWhile_r_tail str (\x -> isDigit x || x == '.')
          func_tail str = takeWhile_r_tail str isLetter
          un_op = create_un_op (fst $ func_tail (c:cs))
          bin_op = create_bin_op c

check_prior :: [Token] -> Int -> [Token]
check_prior [] _ = []
check_prior (t:ts) prior =
    case t of
        LeftBr    -> check_prior ts (prior + 5)
        RightBr   -> check_prior ts (prior - 5)
        UnOp un   -> (UnOp  (inc_prior_un un prior)) : (check_prior ts prior)
        BinOp bin -> (BinOp (inc_prior_bin bin prior)) : (check_prior ts prior)
        _         -> t : (check_prior ts prior)

data Term = BiTerm BinaryOperator Term Term |
            UnTerm UnaryOperator  Term      |
            VarX | VarNumber Double |
            Error
            deriving (Eq)

instance Show Term where
    show (BiTerm op left right) = "(" ++ show left ++ show op ++ show right ++ ")"
    show (UnTerm func op) = show func ++ "(" ++ show op ++ ")"
    show (VarX) = "x"
    show (VarNumber d) = show d

type Function = Term

is_bin_op :: Token -> Bool
is_bin_op (BinOp b) = True
is_bin_op _ = False

is_un_op :: Token -> Bool
is_un_op (UnOp b) = True
is_un_op _ = False

instance Num Term where
    x + y = BiTerm (fromJust $ create_bin_op '+') x y
    x - y = BiTerm (fromJust $ create_bin_op '-') x y
    x * y = BiTerm (fromJust $ create_bin_op '*') x y
    fromInteger i = create_func $ show i

instance Fractional Term where
    x / y = BiTerm (fromJust $ create_bin_op '/') x y
    fromRational r = create_func $ show r

type UnOp_n_Arg  = Maybe (Token, [Token])
type BinOp_n_Arg = Maybe (Token, [Token], [Token])

minimum' :: Ord a => [a] -> Maybe a
minimum' [] = Nothing
minimum' list = Just (minimum list)

find_weak_un_op :: [Token] -> UnOp_n_Arg
find_weak_un_op []     = Nothing
find_weak_un_op tokens =
    case token of
        Just tok -> Just (tok, tail $ snd token_tail)
            where token_tail = takeWhile_r_tail tokens (\x -> x /= tok)
        _        -> Nothing
        where
            token = minimum' (filter is_un_op tokens)

reverse_find :: [Token] -> Token -> ([Token], [Token])
reverse_find tokens tok = (\ (x, y) -> (reverse y, reverse x)) token_tail
    where token_tail = takeWhile_l_tail (reverse tokens) (/= tok)

find_weak_bin_op :: [Token] -> BinOp_n_Arg
find_weak_bin_op []     = Nothing
find_weak_bin_op tokens =
    case token of
        Just tok -> Just (tok, fst token_tail, tail $ snd token_tail)
            where token_tail = reverse_find tokens tok
        _        -> Nothing
        where
            token = minimum' (filter is_bin_op tokens)

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

-- If we have at least one Error in Function, Function is incorrect
terminator :: [Token] -> Function
terminator [] = Error
terminator [X]        = VarX
terminator [Number a] = VarNumber a
terminator tokens =
    case weak_op_ht of
        Left  (Just (UnOp u, tok))          -> UnTerm u (terminator tok)
        Right (Just (BinOp b, tok1, tok2))  -> BiTerm b (terminator tok1) (terminator tok2)
        Left Nothing                        -> Error
    where
        weak_op_ht = find_weak_op tokens


evaluate_func :: Function -> Double -> Double
evaluate_func VarX value                        = value
evaluate_func (VarNumber number) _              = number
evaluate_func (UnTerm un_op term) value         = eval_un  un_op (evaluate_func term value)
evaluate_func (BiTerm bi_op term1 term2) value  = eval_bin bi_op (evaluate_func term1 value) (evaluate_func term2 value)

create_func :: String -> Function
create_func str = terminator $ check_prior (tokenizer str) 0
