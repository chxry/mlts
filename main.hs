import System.IO
import Data.Char
import Data.List
import Data.List.Split

main = loadFile

repl = do
  putStr "> "
  hFlush stdout
  input <- getLine
  runInput input
  repl

loadFile = do
  handle <- openFile "test" ReadMode
  input <- hGetContents handle
  runInput input

runInput x = do
  let lexed = tokenize x
  let parsed = parseProgram lexed
  putStrLn ("lexed: " ++ show lexed)
  putStrLn ("parsed " ++ show parsed)
  putStrLn ("formatted:\n  " ++ printExprs "\n  " parsed)

data Token = TInt Integer | TLabel String | OpenParen | CloseParen
  deriving (Show)

readToken :: String -> Token
readToken "(" = OpenParen
readToken ")" = CloseParen
readToken str | isDigit (head str) = TInt (read str)
              | otherwise = TLabel str

tokenize :: String -> [Token]
tokenize str = fmap readToken (concatMap words (split (oneOf "()") str))

data Expr = Int Integer | Label String | SExpr [Expr]
  deriving (Show)

parseExpr :: [Token] -> (Expr, [Token])
parseExpr (TInt x:xs) = (Int x, xs)
parseExpr (TLabel x:xs) = (Label x, xs)
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
printExpr (Int x) = show x;
printExpr (Label x) = x;
printExpr (SExpr xs) = "(" ++ printExprs " " xs ++ ")"
