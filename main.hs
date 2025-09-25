import System.IO
import Data.Char
import Data.List
import Data.List.Split

-- main = loadFile "test"
main = repl

repl = do
  putStr "> "
  hFlush stdout
  input <- getLine
  runInput input
  repl

loadFile x = do
  handle <- openFile x ReadMode
  input <- hGetContents handle
  runInput input

runInput x = do
  let lexed = tokenize x
  let parsed = parseProgram lexed
  putStrLn ("lexed: " ++ show lexed)
  putStrLn ("parsed: " ++ show parsed)
  putStrLn ("formatted:\n  " ++ printExprs "\n  " parsed)
  putStrLn ("eval:\n  " ++ intercalate "\n  " (fmap (show . eval) parsed))

data Token = TInt Integer | TLabel String | OpenParen | CloseParen
  deriving (Show)

readToken :: String -> Token
readToken "(" = OpenParen
readToken ")" = CloseParen
readToken str | isDigit (head str) = TInt (read str)
              | otherwise = TLabel str

tokenize :: String -> [Token]
tokenize str = fmap readToken (concatMap words (split (oneOf "()") str))

data Expr = EInt Integer | ELabel String | SExpr [Expr]
  deriving (Show)

parseExpr :: [Token] -> (Expr, [Token])
parseExpr (TInt x:xs) = (EInt x, xs)
parseExpr (TLabel x:xs) = (ELabel x, xs)
parseExpr (OpenParen:xs) =
  let (sexp, rest) = parseSExpr xs
  in (SExpr sexp, rest)

parseSExpr :: [Token] -> ([Expr], [Token])
parseSExpr (CloseParen:xs) = ([], xs)
parseSExpr xs =
  let (expr, rest) = parseExpr xs
      (sexp, rest2) = parseSExpr rest
  in (expr:sexp, rest2)

parseProgram :: [Token] -> [Expr]
parseProgram [] = []
parseProgram tokens =
  let (expr, rest) = parseExpr tokens
  in expr:parseProgram rest

printExprs :: String -> [Expr] -> String
printExprs sep xs = intercalate sep (fmap printExpr xs)

printExpr :: Expr -> String
printExpr (EInt x) = show x;
printExpr (ELabel x) = x;
printExpr (SExpr xs) = "(" ++ printExprs " " xs ++ ")"

data Value = VInt Integer | VBool Bool | VLabel String
  deriving (Show)

eval :: Expr -> Value
eval (EInt x) = VInt x
eval (ELabel "true") = VBool True
eval (ELabel "false") = VBool False
eval (ELabel x) = VLabel x
eval (SExpr (x:xs)) = apply (eval x) (fmap eval xs)

apply :: Value -> [Value] -> Value
apply (VLabel "+") [VInt a, VInt b] = VInt (a + b)
apply (VLabel "-") [VInt a, VInt b] = VInt (a - b)
apply (VLabel "*") [VInt a, VInt b] = VInt (a * b)
apply (VLabel "/") [VInt a, VInt b] = VInt (div a b)
apply (VLabel "pow") [VInt a, VInt b] = VInt (a ^ b)
apply (VLabel "mod") [VInt a, VInt b] = VInt (mod a b)

apply (VLabel "==") [VInt a, VInt b] = VBool (a == b)
apply (VLabel "!=") [VInt a, VInt b] = VBool (a /= b)
apply (VLabel ">") [VInt a, VInt b] = VBool (a > b)
apply (VLabel ">=") [VInt a, VInt b] = VBool (a >= b)
apply (VLabel "<") [VInt a, VInt b] = VBool (a < b)
apply (VLabel "<=") [VInt a, VInt b] = VBool (a <= b)

apply (VLabel "and") [VBool a, VBool b] = VBool (a && b)
apply (VLabel "or") [VBool a, VBool b] = VBool (a || b)

apply x _ = error ("invalid operation " ++ show x)
