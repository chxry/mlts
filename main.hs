import System.IO
import Data.Char
import Data.List
import Data.List.Split
import qualified Data.Map as Map

type Context = Map.Map String Value

main :: IO ()
main = loadFile "test"
-- main = repl defaultContext

repl :: Context -> IO ()
repl c = do
  putStr "> "
  hFlush stdout
  input <- getLine
  c' <- runInput c input
  repl c'

loadFile :: String -> IO ()
loadFile x = do
  input <- readFile x
  runInput defaultContext input
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

data Value = VInt Integer | VBool Bool | VList [Value] | VClosure String Expr Context | VPrim (Value -> Value)

printValue :: Value -> String
printValue (VInt x) = show x
printValue (VBool True) = "true"
printValue (VBool False) = "false"
printValue (VList xs) = "(" ++ unwords (map printValue xs)++ ")"
printValue (VClosure b x _) = "(lambda " ++ b ++ " " ++ printExpr x ++ ")"
printValue (VPrim _) = "<primitive>"

evalStmt :: Context -> Expr -> (Context, Value)
evalStmt c (ESExpr [ELabel "define", ELabel l, x]) =
  let val = eval c x
  in (Map.insert l val c, val)
evalStmt c x = (c, eval c x)

eval :: Context -> Expr -> Value
eval c (EInt x) = VInt x
eval c (ELabel "true") = VBool True
eval c (ELabel "false") = VBool False
eval c (ELabel x) =
  case Map.lookup x c of
    Just v -> v
    Nothing -> error ("unknown var " ++ x)
eval c (ESExpr ([ELabel "lambda", ELabel b, x])) = VClosure b x c
eval c (ESExpr (ELabel "list":xs)) = VList (map (eval c) xs)
eval c (ESExpr (f:xs)) = foldl (\g x -> apply g (eval c x)) (eval c f) xs

apply :: Value -> Value -> Value
apply (VClosure b body c') arg = eval (Map.insert b arg c') body
apply (VPrim f) arg = f arg
apply f _ = error ("cannot apply " ++ printValue f)

prim2 :: (Value -> Value -> Value) -> Value
prim2 f = VPrim (\a -> VPrim (\b -> f a b))

prim3 :: (Value -> Value -> Value -> Value) -> Value
prim3 f = VPrim (\a -> VPrim (\b -> VPrim (\c -> f a b c)))

defaultContext :: Context
defaultContext = Map.fromList [
  ("+", prim2 (\(VInt a) (VInt b) -> VInt (a + b))),
  ("-", prim2 (\(VInt a) (VInt b) -> VInt (a - b))),
  ("*", prim2 (\(VInt a) (VInt b) -> VInt (a * b))),
  ("/", prim2 (\(VInt a) (VInt b) -> VInt (div a b))),
  ("pow", prim2 (\(VInt a) (VInt b) -> VInt (a ^ b))),
  ("mod", prim2 (\(VInt a) (VInt b) -> VInt (mod a b))),
  
  ("==", prim2 (\(VInt a) (VInt b) -> VBool (a == b))),
  ("!=", prim2 (\(VInt a) (VInt b) -> VBool (a /= b))),
  (">", prim2 (\(VInt a) (VInt b) -> VBool (a > b))),
  (">=", prim2 (\(VInt a) (VInt b) -> VBool (a >= b))),
  ("<", prim2 (\(VInt a) (VInt b) -> VBool (a < b))),
  ("<=", prim2 (\(VInt a) (VInt b) -> VBool (a <= b))),

  ("and", prim2 (\(VBool a) (VBool b) -> VBool (a && b))),
  ("or", prim2 (\(VBool a) (VBool b) -> VBool (a || b))),
  ("if", prim3 (\(VBool x) a b -> if x then a else b)),

  ("map", prim2 (\f (VList xs) -> VList (map (apply f) xs)))]
