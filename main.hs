import System.IO
import Data.Char
import Data.List
import Data.List.Split
import qualified Data.Map as Map

type Context = Map.Map String Value

main :: IO ()
-- main = loadFile "test"
main = repl Map.empty

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
  let out = mapAccumL evalStmt ctx parsed
  putStrLn ("lexed: " ++ show lexed)
  putStrLn ("parsed: " ++ show parsed)
  putStrLn ("formatted:\n  " ++ intercalate "\n  " (fmap printStmt parsed))
  putStrLn ("eval:\n  " ++ intercalate "\n  " (fmap show (snd out)))
  return (fst out)

data Token = TInt Integer | TLabel String | Define | OpenParen | CloseParen
  deriving (Eq, Show)

readToken :: String -> Token
readToken "(" = OpenParen
readToken ")" = CloseParen
readToken "define" = Define
readToken str | isDigit (head str) = TInt (read str)
              | otherwise = TLabel str

tokenize :: String -> [Token]
tokenize str = fmap readToken (concatMap words (split (oneOf "()") str))

data Expr = EInt Integer | ELabel String | ESExpr [Expr]
  deriving (Show)

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
      (sexp, rest2) = parseSExpr rest
  in (expr:sexp, rest2)

printExpr :: Expr -> String
printExpr (EInt x) = show x;
printExpr (ELabel x) = x;
printExpr (ESExpr xs) = "(" ++ intercalate " " (fmap printExpr xs) ++ ")"

data Stmt = SExpr Expr | SDefine String Expr
  deriving (Show)

parseStmt :: [Token] -> (Stmt, [Token])
parseStmt (OpenParen:Define:TLabel l:xs) =
  let (expr, rest) = parseExpr xs
  in (SDefine l expr, tail rest)

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

data Prim = Add | Sub | Mul | Div | Pow | Mod | Eq | Ne | Gt | Ge | Lt | Le | And | Or
  deriving (Show)
data Value = VInt Integer | VBool Bool | VPrim Prim
  deriving (Show)

evalStmt :: Context -> Stmt -> (Context, Maybe Value)
evalStmt c (SExpr x) = (c, Just (eval c x))
evalStmt c (SDefine l x) = (Map.insert l (eval c x) c, Nothing)

eval :: Context -> Expr -> Value
eval c (EInt x) = VInt x
eval c (ELabel "true") = VBool True
eval c (ELabel "false") = VBool False
eval c (ELabel "+") = VPrim Add
eval c (ELabel "-") = VPrim Sub
eval c (ELabel "*") = VPrim Mul
eval c (ELabel "/") = VPrim Div
eval c (ELabel "pow") = VPrim Pow
eval c (ELabel "mod") = VPrim Mod
eval c (ELabel "==") = VPrim Eq
eval c (ELabel "!=") = VPrim Ne
eval c (ELabel ">") = VPrim Gt
eval c (ELabel ">=") = VPrim Ge
eval c (ELabel "<") = VPrim Lt
eval c (ELabel "<=") = VPrim Le
eval c (ELabel "and") = VPrim And
eval c (ELabel "or") = VPrim Or
eval c (ELabel x) = c Map.! x
eval c (ESExpr (x:xs)) = apply (eval c x) (fmap (eval c) xs)

apply :: Value -> [Value] -> Value
apply (VPrim Add) [VInt a, VInt b] = VInt (a + b)
apply (VPrim Sub) [VInt a, VInt b] = VInt (a - b)
apply (VPrim Mul) [VInt a, VInt b] = VInt (a * b)
apply (VPrim Div) [VInt a, VInt b] = VInt (div a b)
apply (VPrim Pow) [VInt a, VInt b] = VInt (a ^ b)
apply (VPrim Mod) [VInt a, VInt b] = VInt (mod a b)

apply (VPrim Eq) [VInt a, VInt b] = VBool (a == b)
apply (VPrim Ne) [VInt a, VInt b] = VBool (a /= b)
apply (VPrim Gt) [VInt a, VInt b] = VBool (a > b)
apply (VPrim Ge) [VInt a, VInt b] = VBool (a >= b)
apply (VPrim Lt) [VInt a, VInt b] = VBool (a < b)
apply (VPrim Le) [VInt a, VInt b] = VBool (a <= b)

apply (VPrim And) [VBool a, VBool b] = VBool (a && b)
apply (VPrim Or) [VBool a, VBool b] = VBool (a || b)

apply x _ = error ("invalid operation " ++ show x)
