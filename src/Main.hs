module Main where

import           Control.Monad (void)
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Text.Megaparsec.Expr
import qualified Text.Megaparsec.Lexer as L
import           Text.Megaparsec.String (Parser)
import 		 Data.Bifunctor
import 		 System.Console.Haskeline
import           Expression
import           Derivation
import 		 Control.Monad.Except
import           Simplification
import           Integration
import 		 Normalization
import           Data.Char

sc :: Parser () -- ‘sc’ stands for “space consumer”
sc = let lineComment  = L.skipLineComment "//"
         blockComment = L.skipBlockComment "/*" "*/"
     in L.space (void spaceChar) lineComment blockComment
	        
lexeme = L.lexeme sc
symbol = L.symbol sc

integer :: Parser Double
integer = fromIntegral <$> lexeme L.integer   --parser za integer

double :: Parser Double
double = lexeme L.float

parens :: Parser Exp -> Parser Exp
parens = between (symbol "(") (symbol ")")

term =  parens expr -- terminalni simboli
    <|> ENum <$> integer
    <|> ENum <$> double
    <|> EVar <$> symbol "x"
    <|> EVar <$> symbol "y"
    <?> "term"

table = [ [ Prefix (symbol "sin" *> pure ESin)
          , Prefix (symbol "cos" *> pure ECos)
          , Prefix (symbol "log" *> pure ELog)
          , Prefix (symbol "-"   *> pure ENeg)
          , Prefix (symbol "exp" *> pure EExp)
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


evalString :: String -> Either EvalError Exp
evalString s = do
  exp <- first (const Expression.ParseError) $ parseExp s
  return $ simplifyConverge exp

evalDer :: String -> Either EvalError Exp
evalDer s = do
	exp <- first (const Expression.ParseError) $ parseExp s
	e <- derive $ normalize exp
	return  $ simplifyConverge e

evalInt :: String -> Either EvalError Exp
evalInt s = do
	exp <- first (const Expression.ParseError) $ parseExp s
	e <- integrate $ normalize exp
	return $ simplifyConverge e

main :: IO ()
main = runInputT defaultSettings loop
  where
    loop :: InputT IO ()
    loop = do {
      ; minput <- getInputLine "% "
      ; case minput of
          Nothing -> return ()
          Just "quit" -> return ()
          Just input  -> if isDerive (head (words input)) 
          	             then do { case evalDer $ trim (dropWhile (/=' ') input) of
	  				                Left msg -> outputStrLn $ show msg
	  				                Right exp -> outputStrLn $ show exp
	  				            ; loop
	  				            }
						 else if isIntegrate (head (words input)) 
						 	  then do { case evalInt $ trim (dropWhile (/= ' ') input) of
	  				                Left msg -> outputStrLn $ show msg
	  				                Right exp -> outputStrLn $ show exp
	  				            ; loop
	  				            }
          				 else do { case evalString input of
                               Left msg -> outputStrLn $ show msg
                               Right exp -> outputStrLn $ show exp
                           ; loop
                           }
	  }

trim s = dropWhile (flip elem " \t") s

isIntegrate x = elem (map toLower x) ["i", "int", "integ", "integrate"]
isDerive x = elem (map toLower x) ["d", "der", "derive"]

