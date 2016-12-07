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

data Exp  = EAdd Exp Exp
			    | ESub Exp Exp
			    | EMul Exp Exp
			    | ENeg Exp
			    | EDiv Exp Exp
					| EInt Integer
					| ESin Exp
					| ECos Exp
					| ELog Exp
					| EExp Exp
					| Const String--treba Const Int umesto EInt Integer
					--treba Id -> identicka funkcija za f(x) = x
					| EPow Exp Exp 

instance Show Exp where
	show (EAdd expr1 expr2) = show expr1 ++ "+" ++ show expr2
	show (EMul expr1 expr2) = show expr1 ++ "*" ++ show expr2
	show (ESub expr1 expr2) = show expr1 ++ "-" ++ show expr2
	show (EDiv expr1 expr2) = show expr1 ++ "/" ++ show expr2
	show (EPow expr1 expr2) = show expr1 ++ "*" ++ show expr2
	show (ELog expr) = "ln("  ++ show expr ++ ")"
	show (ESin expr) = "sin(" ++ show expr ++ ")"
	show (ECos expr) = "cos(" ++ show expr ++ ")"
	show (ENeg expr) = "-("   ++ show expr ++ ")"
	show (EExp expr) = "e^("  ++ show expr ++ ")"
	show (EInt x)    = show x
	show (Const s)   = s


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
    <|> Const <$> symbol "y"
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


data EvalError = DivideByZero
               | ParseError
                 deriving Show


--prvi patt. match je x' = 1, ali valja ga promeniti. Mozda moze da postoji
-- komanda kojom se zadaje promenljiva po kojoj se sledeci izvod trazi
derive :: Exp -> Either EvalError Exp
derive (Const s) = return $ EInt 1 
derive (EAdd exp1 exp2) = do
  v1 <- derive exp1
  v2 <- derive exp2
  return $ EAdd v1 v2
derive (ESub exp1 exp2) = do
  v1 <- derive exp1
  v2 <- derive exp2
  return $ ESub v1 v2
derive (ENeg expr) = do
	v <- derive expr
	return $ ENeg v
derive (EMul exp1 exp2) = do
  v1 <- derive exp1
  v2 <- derive exp2
  return $ EAdd (EMul v1 exp2) (EMul exp1 v2)
derive (ESin expr) = do
  v <- derive expr
  return $ EMul (ECos expr) v
derive (ECos expr) = do
  v <- derive expr
  return $ EMul (ENeg $ ESin expr) v
derive (ELog expr) = do
  v <- derive expr
  return $ EDiv v expr
derive (EExp expr) = do
	v <- derive expr
	return $ EMul (EExp expr) v
derive (EPow expr1 expr2) = do --netacno
  v2 <- derive expr2
  return $ EMul (EPow expr1 expr2) (EAdd (EMul v2 $ ELog expr1) (EDiv expr2 expr1))
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
	e <- derive exp

	return $ simplify e

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

simplifyBasic :: Exp -> Exp
--Sabiranje  s nulom 
simplifyBasic (EAdd exp (EInt 0)) = exp
simplifyBasic (EAdd (EInt 0) exp) = exp
simplifyBasic (EAdd exp (ENeg exp2)) = ESub exp exp2

-- Oduzimanje nule i od nule
simplifyBasic (ESub exp (EInt 0)) = exp
simplifyBasic (ESub (EInt 0) exp) = ENeg exp --hocemo li ovo?
simplifyBasic (ESub exp1 (ENeg exp2)) = EAdd exp1 exp2

--Mnozenje nulom
simplifyBasic (EMul (EInt 0) exp) = EInt 0
simplifyBasic (EMul exp (EInt 0)) = EInt 0

--Mnozenje i deljenje jedinicom
simplifyBasic (EMul (EInt 1) exp) = exp
simplifyBasic (EMul exp (EInt 1)) = exp
simplifyBasic (EDiv exp (EInt 1)) = exp

-- Stepenovanje jedinicom
-- Dodati stepenovanje nulom, uz neke provere?
simplifyBasic (EPow exp (EInt 1)) = exp

-- Ovde moramo proveriti da li su exp1 i exp2 jednaki; o tom potom
simplifyBasic (EAdd (EPow (ESin exp1) (EInt 2)) (EPow (ECos exp2) (EInt 2))) = (EInt 1)
simplifyBasic (EAdd (EPow (ECos exp1) (EInt 2)) (EPow (ESin exp2) (EInt 2))) = (EInt 1)

-- Ako se ne uklapa u prethodne sablone
simplifyBasic exp = exp

-- razlog zasto sam napravio simplify i simplifyBasic je da ne bi dolazilo do beskonacne rekurzije
simplify :: Exp -> Exp

-- Ovo na dno, specificnije cemo prvo ispitati ako treba
-- Sabiranje
simplify (EAdd exp1 exp2) = 
                          let levi = simplify exp1
                              desni = simplify exp2
                          in  simplifyBasic (EAdd levi desni)

-- Oduzimanje
simplify (ESub exp1 exp2) = 
                          let levi = simplify exp1
                              desni = simplify exp2
                          in  simplifyBasic (ESub levi desni)

-- Mnozenje
simplify (EMul exp1 exp2) = 
                          let levi = simplify exp1
                              desni = simplify exp2
                          in  simplifyBasic (EMul levi desni)

--Deljenje
simplify (EDiv exp1 exp2) = 
                          let levi = simplify exp1
                              desni = simplify exp2
                          in  simplifyBasic (EDiv levi desni)

-- Negacija
simplify (ENeg (ENeg exp)) = exp

-- Ako se ne uklapa u prethodne sablone
simplify exp = exp

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
-- eval (EInt n) = Right n

--prvi patt. match je x' = 1, ali valja ga promeniti. Mozda moze da postoji
-- komanda kojom se zadaje promenljiva po kojoj se sledeci izvod trazi



-- MOZDA treba rastaviti na binarne i unarne operatore, da bi ovakve funkcije bile jednostavnije.
-- U tutorijalu rade nesto slicno. Negde ce da zakomplikuje, a negde ce biti jednostavnije.
dubinaDrveta :: Exp -> Int
dubinaDrveta e = 1