module Expression where

data Exp  = EAdd Exp Exp
			    | ESub Exp Exp
			    | EMul Exp Exp
			    | ENeg Exp
			    | EDiv Exp Exp
					| ENum Double
					| ESin Exp
					| ECos Exp
					| ELog Exp
					| EExp Exp
					| EId String
					| EPow Exp Exp
					   deriving Eq

data (Num a) => ENum1 a = ENum1 a | Nista deriving Show

data EvalError = DivideByZero
               | ParseError
                 deriving Show


instance Show Exp where
	show (EAdd expr1 expr2) = show expr1 ++ "+" ++ show expr2
	show (EMul expr1 expr2) = show expr1 ++ "*" ++ show expr2
	show (ESub expr1 expr2) = show expr1 ++ "-" ++ show expr2
	show (EDiv expr1 expr2) = show expr1 ++ "/" ++ show expr2
	show (EPow expr1 expr2) = show expr1 ++ "*" ++ show expr2
	show (ELog expr) = "ln("  ++ show expr ++ ")"
	show (ESin expr) = "sin(" ++ show expr ++ ")"
	show (ECos expr) = "cos(" ++ show expr ++ ")"
	show (ENeg expr) = "-("  ++ show expr ++ ")"
	-- show (ENeg (ECos expr)) = "-"   ++ show expr
	-- show (ENeg (ESin expr)) = "-"   ++ show expr
	-- show (ENeg (ESin expr)) = "-"   ++ show expr
	-- show (ENeg (ESin expr)) = "-"   ++ show expr
	-- show (ENeg (ESin expr)) = "-"   ++ show expr
	-- show (ENeg (ESin expr)) = "-"   ++ show expr
	show (EExp expr) = "e^("  ++ show expr ++ ")"
	show (ENum x)    = show x
	show (EId s)   = s
