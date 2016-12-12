module Simplification where

import Expression

simplifyBasic :: Exp -> Exp
--Sabiranje  s nulom 
simplifyBasic (EAdd exp (ENum 0)) = exp
simplifyBasic (EAdd (ENum 0) exp) = exp
simplifyBasic (EAdd exp (ENeg exp2)) = ESub exp exp2

-- Oduzimanje nule i od nule
simplifyBasic (ESub exp (ENum 0)) = exp
simplifyBasic (ESub (ENum 0) exp) = ENeg exp 
simplifyBasic (ESub exp1 (ENeg exp2)) = EAdd exp1 exp2

--Mnozenje nulom
simplifyBasic (EMul (ENum 0) exp) = ENum 0
simplifyBasic (EMul exp (ENum 0)) = ENum 0

--Mnozenje i deljenje jedinicom
simplifyBasic (EMul (ENum 1) exp) = exp
simplifyBasic (EMul exp (ENum 1)) = exp
simplifyBasic (EDiv exp (ENum 1)) = exp

-- Stepenovanje jedinicom
-- Dodati stepenovanje nulom, uz neke provere?
simplifyBasic (EPow exp (ENum 1)) = exp

simplifyBasic (ESin (ENeg exp)) = ENeg (ESin exp)
simplifyBasic (ECos (ENeg exp)) = ECos exp

-- Ovde moramo proveriti da li su exp1 i exp2 jednaki bolje; o tom potom
simplifyBasic (EAdd (EPow (ESin exp1) (ENum 2)) (EPow (ECos exp2) (ENum 2))) = 
            if (exp1 == exp2) then (ENum 1) 
            else (EAdd (EPow (ESin exp1) (ENum 2)) (EPow (ECos exp2) (ENum 2)))
simplifyBasic (EAdd (EPow (ECos exp1) (ENum 2)) (EPow (ESin exp2) (ENum 2))) = 
            if (exp1 == exp2) then (ENum 1) 
            else (EAdd (EPow (ECos exp1) (ENum 2)) (EPow (ESin exp2) (ENum 2)))

simplifyBasic (EDiv (ENeg exp1) (ENeg exp2)) = (EDiv exp1 exp2)

simplifyBasic (EDiv exp1 exp2) = 
            if (exp1 == exp2) then (ENum 1)
            else (EDiv exp1 exp2)

simplifyBasic (ENeg (ENeg exp)) = simplify exp

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
simplify (ENeg exp) = simplifyBasic (ENeg $ simplify exp)


-- Ako se ne uklapa u prethodne sablone
simplify exp = simplifyBasic exp

--sw exp = if x == simplify x then x else sw (simplify x)