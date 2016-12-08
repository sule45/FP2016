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
					| EVar String
					| EPow Exp Exp
					   deriving Eq


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
	----------------
	show (ENeg (ECos expr)) = "-"   ++ show (ECos expr)
	show (ENeg (ESin expr)) = "-"   ++ show (ESin expr)
	show (ENeg (ELog expr)) = "-"   ++ show (ELog expr)
	show (ENeg (EExp expr)) = "-"   ++ show (EExp expr)
	show (ENeg (EVar var))  = "-"   ++ show var
	show (ENeg (ENum num))  = "-"   ++ show num
	show (ENeg expr)        = "-("  ++ show expr ++ ")"
	show (EExp expr)       = "e^("  ++ show expr ++ ")"
	show (ENum x)   = show x
	show (EVar s)   = s
