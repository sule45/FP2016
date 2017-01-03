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
               | IntegrationFailure
                 deriving Show


instance Show Exp where
	-- neki spec slucajevi za mnozenje i deljenje 
	show (EMul m1@(EAdd exp1 exp2) m2@(EAdd exp3 exp4)) = "(" ++ show m1 ++ ")*("  ++ show m2 ++ ")" 
	show (EMul m1 m2@(EAdd exp3 exp4)) = show m1 ++ "*("  ++ show m2 ++ ")" 
	show (EMul m1@(EAdd exp1 exp2) m2) = "(" ++ show m1 ++ ")*" ++ show m2 

	show (EMul m1@(ESub exp1 exp2) m2@(ESub exp3 exp4)) = "(" ++ show m1 ++ ")*("  ++ show m2 ++ ")" 
	show (EMul m1 m2@(ESub exp3 exp4)) = show m1 ++ "*("  ++ show m2 ++ ")" 
	show (EMul m1@(ESub exp1 exp2) m2) = "(" ++ show m1 ++ ")*" ++ show m2 

	show (EDiv m1@(EAdd exp1 exp2) m2@(EAdd exp3 exp4)) = "(" ++ show m1 ++ ")/("  ++ show m2 ++ ")" 
	show (EDiv m1 m2@(EAdd exp3 exp4)) = show m1 ++ "/("  ++ show m2 ++ ")" 
	show (EDiv m1@(EAdd exp1 exp2) m2) = "(" ++ show m1 ++ ")/" ++ show m2 

	show (EDiv m1@(ESub exp1 exp2) m2@(ESub exp3 exp4)) = "(" ++ show m1 ++ ")/("  ++ show m2 ++ ")" 
	show (EDiv m1 m2@(ESub exp3 exp4)) = show m1 ++ "/("  ++ show m2 ++ ")" 
	show (EDiv m1@(ESub exp1 exp2) m2) = "(" ++ show m1 ++ ")/" ++ show m2 

	-- uopsteno
	show (EAdd expr1 expr2) = show expr1 ++ "+" ++ show expr2
	show (EMul expr1 expr2) = show expr1 ++ "*" ++ show expr2
	show (ESub expr1 expr2) = show expr1 ++ "-" ++ show expr2
	show (EDiv expr1 expr2) = show expr1 ++ "/" ++ show expr2
	-- dodao zagrade jer me mrzelo da pisem sve slucajeve kad treba da se dodaju
	show (EPow expr1 (ENum a)) = "(" ++ show expr1 ++ ")^" ++ show a
	show (EPow expr1 expr2) = "(" ++ show expr1 ++ ")^(" ++ show expr2 ++ ")"
	show (ELog expr) = "ln("  ++ show expr ++ ")"
	show (ESin expr) = "sin(" ++ show expr ++ ")"
	show (ECos expr) = "cos(" ++ show expr ++ ")"
	----------------
	show (ENeg (ECos expr)) = "-"   ++ show (ECos expr)
	show (ENeg (ESin expr)) = "-"   ++ show (ESin expr)
	show (ENeg (ELog expr)) = "-"   ++ show (ELog expr)
	show (ENeg (EExp expr)) = "-"   ++ show (EExp expr)
	show (ENeg (EVar var))  = "-"   ++ show (EVar var)
	show (ENeg (ENum num))  = "-"   ++ show num
	show (ENeg expr)        = "-("  ++ show expr ++ ")"
	show (EExp expr)       = "e^("  ++ show expr ++ ")"
	show (ENum x)   = show x
	show (EVar s)   =  s
