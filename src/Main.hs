module Main where

import           Control.Monad (void)
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Text.Megaparsec.Expr
import qualified Text.Megaparsec.Lexer as L
import           Text.Megaparsec.String (Parser)
import 			     Data.Bifunctor
import 			     System.Console.Haskeline
import           Expression
import           Derivation
import 			     Control.Monad.Except
import           Simplification
import           Integration
import 			 Normalization

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
  return $ sw $ normalize exp

evalDer :: String -> Either EvalError Exp
evalDer s = do
	exp <- first (const Expression.ParseError) $ parseExp s
	e <- derive $ normalize exp
	return  $ sw e

evalInt :: String -> Either EvalError Exp
evalInt s = do
	exp <- first (const Expression.ParseError) $ parseExp s
	e <- integrate $ normalize exp
	return $ sw e

main :: IO ()
main = runInputT defaultSettings loop
  where
    loop :: InputT IO ()
    loop = do {
      ; minput <- getInputLine "% "
      ; case minput of
          Nothing -> return ()
          Just "quit" -> return ()
          Just ('d':'e':'r':rest) -> do { case evalDer $ trim rest of
	  				                Left msg -> outputStrLn $ show msg
	  				                Right exp -> outputStrLn $ show exp
	  				            ; loop
	  				            }
          Just ('i':'n':'t':rest) -> do { case evalInt $ trim rest of
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

-- trenutno ne radi nista ova funkija
-- eval :: Exp -> Either EvalError Integer
-- eval (EAdd exp1 exp2) = do
--   v1 <- eval exp1
--   v2 <- eval exp2
--   return $ v1 + v2
-- eval (ESub exp1 exp2) = do
--   v1 <- eval exp1
--   v2 <- eval exp2
--   return $ v1 - v2
-- eval (EMul exp1 exp2) = do
--   v1 <- eval exp1
--   v2 <- eval exp2
--   return $ v1 * v2
-- eval (EPow exp1 exp2) = do
--   v1 <- eval exp1
--   v2 <- eval exp2
--   return $ v1^v2
-- eval (EDiv exp1 exp2) = do
--   dividend <- eval exp1
--   divisor <- eval exp2
--   if divisor == 0
--     then throwError DivideByZero
--     else return $ dividend `div` divisor
-- eval (ENum n) = Right n

--prvi patt. match je x' = 1, ali valja ga promeniti. Mozda moze da postoji
-- komanda kojom se zadaje promenljiva po kojoj se sledeci izvod trazi



-- MOZDA treba rastaviti na binarne i unarne operatore, da bi ovakve funkcije bile jednostavnije.
-- U tutorijalu rade nesto slicno. Negde ce da zakomplikuje, a negde ce biti jednostavnije.
dubinaDrveta :: Exp -> Int
dubinaDrveta e = 1


trim s = dropWhile (flip elem " \t") s
testSimplify x = sw x == (simplify x)