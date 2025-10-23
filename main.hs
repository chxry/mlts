import System.IO
import Data.Char
import Data.List
import Data.List.Split
import qualified Data.Map as Map

type Context = Map.Map String Value

main :: IO ()
main = loadFile "test"
-- main = repl Map.empty

repl :: Context -> IO ()
repl c = do
  putStr "> "
  hFlush stdout
  input <- getLine
  c' <- runInput c input
  repl c'

loadFile :: String -> IO ()
loadFile x = do
  handle <- openFile x ReadMode
  input <- hGetContents handle
  runInput Map.empty input
  return ()

runInput :: Context -> String -> IO Context
runInput c x = do
  let lexed = tokenize x
  let parsed = parseProgram lexed
  let (c', vals) = mapAccumL evalStmt c parsed
  -- putStrLn ("lexed: " ++ show lexed)
  -- putStrLn ("parsed: " ++ show parsed)
  -- putStrLn (intercalate "\n" ("formatted: ":map printExpr parsed))
  putStrLn (intercalate "\n" (map printValue vals))
  return c'

data Token = TInt Integer | TLabel String | OpenParen | CloseParen
  deriving (Eq, Show)

readToken :: String -> Token
readToken "(" = OpenParen
readToken ")" = CloseParen
readToken str | isDigit (head str) = TInt (read str)
              | otherwise = TLabel str

tokenize :: String -> [Token]
tokenize str = map readToken (concatMap words (split (oneOf "()") str))

data Expr = EInt Integer | ELabel String | ESExpr [Expr]
  deriving (Eq, Show)

parseExpr :: [Token] -> (Expr, [Token])
parseExpr (TInt x:xs) = (EInt x, xs)
parseExpr (TLabel x:xs) = (ELabel x, xs)
parseExpr (OpenParen:xs) =
  let (sexp, rest) = parseSExpr xs
  in (ESExpr sexp, rest)

parseSExpr :: [Token] -> ([Expr], [Token])
parseSExpr (CloseParen:xs) = ([], xs)
parseSExpr xs =
  let (expr, rest) = parseExpr xs
      (sexp, rest') = parseSExpr rest
  in (expr:sexp, rest')

printExpr :: Expr -> String
printExpr (EInt x) = show x;
printExpr (ELabel x) = x;
printExpr (ESExpr xs) = "(" ++ unwords (map printExpr xs) ++ ")"

parseProgram :: [Token] -> [Expr]
parseProgram [] = []
parseProgram tokens =
  let (expr, rest) = parseExpr tokens
  in expr:parseProgram rest

data Value = VInt Integer | VBool Bool | VList [Value] | VClosure String Expr Context
  deriving (Show)

printValue :: Value -> String
printValue (VInt x) = show x
printValue (VBool True) = "true"
printValue (VBool False) = "false"
printValue (VList xs) = "(" ++ unwords (map printValue xs)++ ")"
printValue (VClosure b x _) = "(lambda " ++ b ++ " " ++ printExpr x ++ ")"

evalStmt :: Context -> Expr -> (Context, Value)
evalStmt c (ESExpr [ELabel "define", ELabel l, x]) =
  let val = eval c x
  in (Map.insert l val c, val)
evalStmt c x = (c, eval c x)

eval :: Context -> Expr -> Value
eval c (EInt x) = VInt x
eval c (ELabel "true") = VBool True
eval c (ELabel "false") = VBool False
eval c (ELabel x) = c Map.! x
eval c (ESExpr ([ELabel "lambda", ELabel b, x])) = VClosure b x c
eval c (ESExpr (x:xs)) = apply c x (map (eval c) xs)

-- todo: make these all closures, and curried. then move logic up to eval
apply :: Context -> Expr -> [Value] -> Value
apply c (ELabel "+") [VInt a, VInt b] = VInt (a + b)
apply c (ELabel "-") [VInt a, VInt b] = VInt (a - b)
apply c (ELabel "*") [VInt a, VInt b] = VInt (a * b)
apply c (ELabel "/") [VInt a, VInt b] = VInt (div a b)
apply c (ELabel "pow") [VInt a, VInt b] = VInt (a ^ b)
apply c (ELabel "mod") [VInt a, VInt b] = VInt (mod a b)
apply c (ELabel "modmul-inv") [VInt a, VInt b] = VInt (modmulInv a b)
apply c (ELabel "lcm") [VInt a, VInt b] = VInt (lcm a b)

apply c (ELabel "==") [VInt a, VInt b] = VBool (a == b)
apply c (ELabel "!=") [VInt a, VInt b] = VBool (a /= b)
apply c (ELabel ">") [VInt a, VInt b] = VBool (a > b)
apply c (ELabel ">=") [VInt a, VInt b] = VBool (a >= b)
apply c (ELabel "<") [VInt a, VInt b] = VBool (a < b)
apply c (ELabel "<=") [VInt a, VInt b] = VBool (a <= b)

apply c (ELabel "and") [VBool a, VBool b] = VBool (a && b)
apply c (ELabel "or") [VBool a, VBool b] = VBool (a || b)
apply c (ELabel "if") [VBool x, a, b] = if x then a else b

apply c (ELabel "list") xs = VList xs
apply c (ELabel "map") [v@(VClosure _ _ _), VList xs] = VList (map (applyClosure v) xs)

apply c f [x] = applyClosure (eval c f) x
apply c f _ = error ("invalid operation " ++ show f)

applyClosure :: Value -> Value -> Value
applyClosure (VClosure b x c) v = eval (Map.insert b v c) x

egcd :: Integer -> Integer -> (Integer, Integer, Integer)
egcd a 0 = (a, 1, 0)
egcd a b =
  let (g, x, y) = egcd b (mod a b)
  in (g, y, x - (div a b) * y)

modmulInv :: Integer -> Integer -> Integer
modmulInv a m =
  let (g, x, _) = egcd a m
  in mod x m

