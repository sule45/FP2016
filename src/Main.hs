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
	| ESin Exp
	| ECos Exp
	| Const String
	| EPow Exp Exp
		deriving Show


sc :: Parser () -- ‘sc’ stands for “space consumer”
sc = let lineComment  = L.skipLineComment "//"
         blockComment = L.skipBlockComment "/*" "*/"
     in L.space (void spaceChar) lineComment blockComment
	        
lexeme = L.lexeme sc
symbol = L.symbol sc

integer :: Parser Integer
integer = lexeme L.integer   --parser za integer

parens :: Parser Exp -> Parser Exp
parens = between (symbol "(") (symbol ")")

term =  parens expr -- terminalni simboli
    <|> EInt <$> integer
    <|> Const <$> symbol "x"
    <?> "term"

table = [ [ Prefix (symbol "sin" *> pure ESin)
          , Prefix (symbol "cos" *> pure ECos)
          ]
        , [binary "^" EPow]
		, [binary "*" EMul, binary "/" EDiv]
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

eval :: Exp -> Either EvalError Integer
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
eval (EPow exp1 exp2) = do
  v1 <- eval exp1
  v2 <- eval exp2
  return $ v1^v2
eval (EDiv exp1 exp2) = do
  dividend <- eval exp1
  divisor <- eval exp2
  if divisor == 0
    then throwError DivideByZero
    else return $ dividend `div` divisor
eval (EInt n) = Right n


derive :: Exp -> Either EvalError Exp
derive (Const s) = return $ EInt 1
derive (EAdd exp1 exp2) = do
  v1 <- derive exp1
  v2 <- derive exp2
  return $ EAdd v1 v2
derive (ESub exp1 exp2) = do
  v1 <- derive exp1
  v2 <- derive exp2
  return $ ESub v1  v2
derive (EMul exp1 exp2) = do
  v1 <- derive exp1
  v2 <- derive exp2
  return $ EAdd (EMul v1 exp2) (EMul exp1 v2)
derive (ESin expr) = do
  v <- derive expr
  return $ EMul (ECos expr) v
derive (EPow exp1 exp2) = do --netacno
  v1 <- derive exp1
  v2 <- derive exp2
  return $ EAdd v1 v2
derive (EDiv exp1 exp2) = do
  dividend <- derive exp1
  divisor  <- derive exp2 --mozda neka greska za deljenje se pojavi
  return $ EDiv (ESub (EMul exp1 divisor) (EMul exp2 dividend)) (EPow exp2 (EInt 2))
derive (EInt n) = return $ EInt 0

evalString :: String -> Either EvalError Exp
evalString s = do
  exp <- first (const Main.ParseError) $ parseExp s
  return exp


evalDer :: String -> Either EvalError Exp
evalDer s = do
	exp <- first (const Main.ParseError) $ parseExp s
	derive exp

main :: IO ()
main = runInputT defaultSettings loop
  where
    loop :: InputT IO ()
    loop = do {
      ; minput <- getInputLine "% "
      ; case minput of
          Nothing -> return ()
          Just "quit" -> return ()
          Just ('d':rest) -> do { case evalDer rest of
	  				                Left msg -> outputStrLn $ show msg
	  				                Right exp -> outputStrLn $ show exp
	  				            ; loop
	  				            }
          Just input -> do { case evalString input of
                               Left msg -> outputStrLn $ show msg
                               Right exp -> outputStrLn $ show exp
                           ; loop
                           }
	  }