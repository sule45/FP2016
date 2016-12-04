module FExpr where

import Control.Monad (void)
import Text.Megaparsec
import Text.Megaparsec.Expr 
import Text.Megaparsec.String

import qualified Text.Megaparsec.Lexer as L

data FExpr = Add FExpr FExpr
           | Sub FExpr FExpr
           | Mul FExpr FExpr
           | Div FExpr FExpr
           | Sin FExpr
           | Neg FExpr
           | Const -- f(x) = x
           | Pow Int -- f(x) = x^n
           | Exp FExpr
           | Val Int --Treba Promeniti
-- mozda dodati pi kao FExpr

data Stmt = --Assign String FExpr
            Derive FExpr
          | Print FExpr

sc :: Parser ()
sc = L.space (void spaceChar) lineCmnt mempty
        where lineCmnt  = L.skipLineComment "//"
              
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

integer :: Parser Integer
integer = lexeme L.integer

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

rword :: String -> Parser ()
rword w = string w *> notFollowedBy alphaNumChar *> sc

rws :: [String]
rws = ["sin", "cos", "print", "derive", "x", "t"]

--parser celog jezika, treba ga preimenovati
whileParser :: Parser Stmt
whileParser = sc *> stmt <* eof

stmt :: Parser Stmt 
stmt = parens stmt <|> stmtSeq

stmtSeq :: Parser Stmt
stmtSeq = f <$> sepBy1 stmt' $ symbol "\n"
			where f ss = if length ss == 1 
							then head ss
							else Seq ss

stmt' :: Parser Stmt
stmt' = deriveStmt <|> printStmt

deriveStmt :: Parser Stmt
deriveStmt = do rword "derive"
                e <- fExprParser
                return $ derive e


--printStmt na String-u, da ispise expr koji je u promenljivoj


fExpr :: Parser FExpr
fExpr = makeExprParser fTerm fOperators

fOperators :: [[Operator Parser FExpr]]
fOperators = [ [ Prefix (symbol "-" *> pure Neg)
               ]
             , [ InfixL (symbol "^" *> pure Pow)
               ]  
             , [ InfixL (symbol "*" *> pure Mul)
               , InfixL (symbol "/" *> pure Div)
               ]
             , [ InfixL (symbol "+" *> pure Add)
               , InfixL (symbol "-" *> pure Sub)
               ]
             ]




--fTerm :: Parser FExpr








