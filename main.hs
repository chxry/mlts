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
repl ctx = do
  putStr "> "
  hFlush stdout
  input <- getLine
  ctx2 <- runInput ctx input
  repl ctx2

loadFile :: String -> IO ()
loadFile x = do
  handle <- openFile x ReadMode
  input <- hGetContents handle
  runInput Map.empty input
  return ()

runInput :: Context -> String -> IO Context
runInput ctx x = do
  let lexed = tokenize x
  let parsed = parseProgram lexed
  let (newCtx, vals) = mapAccumL evalStmt ctx parsed
  -- putStrLn ("lexed: " ++ show lexed)
  -- putStrLn ("parsed: " ++ show parsed)
  -- putStrLn ("formatted:\n  " ++ intercalate "\n  " (map printStmt parsed))
  -- putStrLn ("eval:\n  " ++ intercalate "\n  " (map show vals))
  putStrLn (intercalate "\n" (map printValue vals))
  return newCtx

data Token = TInt Integer | TLabel String | Define | Lambda | OpenParen | CloseParen
  deriving (Eq, Show)

readToken :: String -> Token
readToken "(" = OpenParen
readToken ")" = CloseParen
readToken "define" = Define
readToken "lambda" = Lambda
readToken str | isDigit (head str) = TInt (read str)
              | otherwise = TLabel str

tokenize :: String -> [Token]
tokenize str = map readToken (concatMap words (split (oneOf "()") str))

data Expr = EInt Integer | ELabel String | ESExpr [Expr] | ELambda String Expr
  deriving (Show)

parseExpr :: [Token] -> (Expr, [Token])
parseExpr (TInt x:xs) = (EInt x, xs)
parseExpr (TLabel x:xs) = (ELabel x, xs)
parseExpr (OpenParen:Lambda:TLabel b:xs) =
  let (expr, CloseParen:rest) = parseExpr xs
  in (ELambda b expr, rest)
parseExpr (OpenParen:xs) =
  let (sexp, rest) = parseSExpr xs
  in (ESExpr sexp, rest)

parseSExpr :: [Token] -> ([Expr], [Token])
parseSExpr (CloseParen:xs) = ([], xs)
parseSExpr xs =
  let (expr, rest) = parseExpr xs
      (sexp, rest2) = parseSExpr rest
  in (expr:sexp, rest2)

printExpr :: Expr -> String
printExpr (EInt x) = show x;
printExpr (ELabel x) = x;
printExpr (ESExpr xs) = "(" ++ intercalate " " (map printExpr xs) ++ ")"
printExpr (ELambda b x) = "(lambda " ++ b ++ " " ++ printExpr x ++ ")"

data Stmt = SExpr Expr | SDefine String Expr
  deriving (Show)

parseStmt :: [Token] -> (Stmt, [Token])
parseStmt (OpenParen:Define:TLabel l:xs) =
  let (expr, CloseParen:rest) = parseExpr xs
  in (SDefine l expr, rest)

parseStmt xs =
  let (expr, rest) = parseExpr xs
  in (SExpr expr, rest)

parseProgram :: [Token] -> [Stmt]
parseProgram [] = []
parseProgram tokens =
  let (stmt, rest) = parseStmt tokens
  in stmt:parseProgram rest

printStmt :: Stmt -> String
printStmt (SExpr x) = printExpr x
printStmt (SDefine l x) = "(define " ++ l ++ " " ++ printExpr x ++ ")"

data Value = VInt Integer | VBool Bool | VClosure String Expr Context
  deriving (Show)

printValue :: Value -> String
printValue (VInt x) = show x
printValue (VBool True) = "true"
printValue (VBool False) = "false"
printValue (VClosure b x _) = printExpr (ELambda b x)

evalStmt :: Context -> Stmt -> (Context, Value)
evalStmt c (SExpr x) = (c, eval c x)
evalStmt c (SDefine l x) =
  let val = eval c x
  in (Map.insert l val c, val)

eval :: Context -> Expr -> Value
eval c (EInt x) = VInt x
eval c (ELabel "true") = VBool True
eval c (ELabel "false") = VBool False
eval c (ELabel x) = c Map.! x
eval c (ESExpr (x:xs)) = apply c x (map (eval c) xs)
eval c (ELambda b x) = VClosure b x c

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
apply c l [expr] =
  let (VClosure b x newC) = eval c l
  in eval (Map.insert b expr newC) x
apply c x _ = error ("invalid operation " ++ show x)

egcd :: Integer -> Integer -> (Integer, Integer, Integer)
egcd a 0 = (a, 1, 0)
egcd a b =
  let (g, x, y) = egcd b (mod a b)
  in (g, y, x - (div a b) * y)

modmulInv :: Integer -> Integer -> Integer
modmulInv a m =
  let (g, x, _) = egcd a m
  in mod x m

