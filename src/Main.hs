module Main where


import           Control.Monad (void)
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Text.Megaparsec.Expr
import qualified Text.Megaparsec.Lexer as L
import           Text.Megaparsec.String (Parser)
import 			 Data.Bifunctor
import 			 System.Console.Haskeline

import 			 Control.Monad.Except

data Exp =
      EAdd Exp Exp
    | ESub Exp Exp
    | EMul Exp Exp
    | EDiv Exp Exp
	| EInt Integer



sc :: Parser () -- ‘sc’ stands for “space consumer”
sc = let lineComment  = L.skipLineComment "//"
         blockComment = L.skipBlockComment "/*" "*/"
     in L.space (void spaceChar) lineComment blockComment
	        
lexeme = L.lexeme sc
symbol = L.symbol sc

integer :: Parser Integer
integer = lexeme L.integer

parens :: Parser Exp -> Parser Exp
parens = between (symbol "(") (symbol ")")

term =  parens expr
    <|> EInt <$> integer
    <?> "term"

table = [ [binary "*" EMul, binary "/" EDiv]
        , [binary "+" EAdd, binary "-" ESub]
        ]

binary name f = InfixL (symbol name >> return f)

expr :: Parser Exp
expr = makeExprParser term table <?> "expression"

--parseExp :: String -> Either ParseError Exp
parseExp = parse expr "" 


data EvalError = DivideByZero
               | ParseError
                 deriving Show

--eval :: Exp -> Either EvalError Integer
eval (EAdd exp1 exp2) = do
  v1 <- eval exp1
  v2 <- eval exp2
  return $ v1 + v2
eval (ESub exp1 exp2) = do
  v1 <- eval exp1
  v2 <- eval exp2
  return $ v1 - v2
eval (EMul exp1 exp2) = do
  v1 <- eval exp1
  v2 <- eval exp2
  return $ v1 * v2
eval (EDiv exp1 exp2) = do
  dividend <- eval exp1
  divisor <- eval exp2
  if divisor == 0
    then throwError DivideByZero
    else return $ dividend `div` divisor
eval (EInt n) = Right n

evalString :: String -> Either EvalError Integer
evalString s = do
  exp <- first (const Main.ParseError) $ parseExp s
  eval exp

main :: IO ()
main = runInputT defaultSettings loop
  where
    loop :: InputT IO ()
    loop = do
      minput <- getInputLine "% "
      case minput of
        Nothing -> return ()
        Just "quit" -> return ()
        Just input -> do
          case evalString input of
            Left msg -> outputStrLn $ show msg
            Right exp -> outputStrLn $ show exp
	  loop