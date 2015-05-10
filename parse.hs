import System.Environment
import Language.Haskell.Interpreter

create_func :: String -> Double -> String
create_func [] _ = ""
create_func (c:cs) x
    | (c == 'x') = show(x) ++ (create_func cs x)
    | otherwise = c : create_func cs x

eval_func :: MonadInterpreter m => String -> m (Either InterpreterError String)
eval_func str = runInterpreter $ setImports ["Prelude"] >> interpret str (as :: String)

f :: MonadInterpreter m => String -> m (Either InterpreterError String)
f str = eval_func $ create_func str 10

main :: IO ()
main = getArgs >>= print . f . head
