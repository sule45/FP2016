module Simplification where

import Expression
import Normalization

simplifyBasic :: Exp -> Exp
--Sabiranje  s nulom 
simplifyBasic (EAdd exp (ENum 0)) = exp
simplifyBasic (EAdd (ENum 0) exp) = exp

simplifyBasic (EAdd (ENum x) (ENum y)) = ENum (x+y)


-- Oduzimanje nule i od nule
simplifyBasic (ESub exp (ENum 0)) = exp
simplifyBasic (ESub (ENum 0) exp) = ENeg exp 
simplifyBasic (ESub exp1 (ENeg exp2)) = EAdd exp1 exp2

simplifyBasic (ESub (ENum x) (ENum y)) = ENum (x-y)

--Mnozenje nulom
simplifyBasic (EMul (ENum 0) exp) = ENum 0
simplifyBasic (EMul exp (ENum 0)) = ENum 0


--Mnozenje i deljenje jedinicom
simplifyBasic (EMul (ENum 1) exp) = exp
simplifyBasic (EMul exp (ENum 1)) = exp
simplifyBasic (EDiv exp (ENum 1)) = exp

simplifyBasic (EMul (ENum x) (ENum y)) = ENum (x*y)
simplifyBasic (EDiv (ENum x) (ENum y)) = ENum (x/y)

-- Stepenovanje jedinicom
-- Dodati stepenovanje nulom, uz neke provere?
simplifyBasic (EPow exp (ENum 1)) = exp

-- Neparnost sinusa i parnost kosinusa
simplifyBasic (ESin (ENeg exp)) = ENeg (ESin exp)
simplifyBasic (ECos (ENeg exp)) = ECos exp

-- sin(x)^2 + cos(x)^2 = 1
-- Ovde moramo proveriti da li su exp1 i exp2 jednaki bolje; o tom potom
simplifyBasic (EAdd (EPow (ESin exp1) (ENum 2)) (EPow (ECos exp2) (ENum 2))) = 
                            if (exp1 == exp2) then (ENum 1) 
                            else (EAdd (EPow (ESin exp1) (ENum 2)) (EPow (ECos exp2) (ENum 2)))
simplifyBasic (EAdd (EPow (ECos exp1) (ENum 2)) (EPow (ESin exp2) (ENum 2))) = 
                            if (exp1 == exp2) then (ENum 1) 
                            else (EAdd (EPow (ECos exp1) (ENum 2)) (EPow (ESin exp2) (ENum 2)))

-- Skracivanje minusa pri deljenju i mnozenju
simplifyBasic (EDiv (ENeg exp1) (ENeg exp2)) = (EDiv exp1 exp2)
simplifyBasic (EMul (ENeg exp1) (ENeg exp2)) = (EMul exp1 exp2)

-- f(x)/f(x) se ponasa kao 1 (sem u nulama funkcije f)
simplifyBasic (EDiv exp1 exp2) = 
            if (exp1 == exp2) then (ENum 1)
            else (EDiv exp1 exp2)

-- Negacija negativnog broja ; razmisliti da li ovo zadrzavamo
simplifyBasic (ENeg (ENum num)) = ENum (-num)
simplifyBasic (ENeg (ENeg exp)) = simplify exp

-- Minus izvlacimo ispred zagrada
simplifyBasic (EMul (ENeg exp1) exp2) = (ENeg (EMul exp1 exp2))
simplifyBasic (EMul exp1 (ENeg exp2)) = (ENeg (EMul exp1 exp2))

simplifyBasic (EAdd (ENeg exp1) (ENeg exp2)) = (ENeg (EAdd  exp1 exp2))
-- (-a-(-b)) = -(a-b) ; koristiti ovo?
--simplifyBasic (ESub (ENeg exp1) (ENeg exp2)) = (ENeg (ESub exp1 exp2))
-- ako se negacija nadje u zbiru, pretvaramo to u razliku
simplifyBasic (EAdd exp1 (ENeg exp2)) = ESub exp1 exp2
simplifyBasic (EAdd (ENeg exp1) exp2) = ESub exp2 exp1

-- Ako se ne uklapa u prethodne sablone
simplifyBasic exp = exp

-- razlog zasto sam napravio simplify i simplifyBasic je da ne bi dolazilo do beskonacne rekurzije
simplify :: Exp -> Exp

--skracivanje konstanti u izrazima
simplify (EAdd (ENum a) (EAdd (ENum b) exp)) = simplifyBasic $ EAdd (ENum (a+b)) (simplify exp)
simplify (EAdd (EAdd (ENum a) exp1) (EAdd (ENum b) exp2)) = simplifyBasic (EAdd (ENum (a+b)) (simplify (EAdd (simplify exp1) (simplify exp2))))

simplify e@(EAdd (EMul (ENum a) exp1) (EMul (ENum b) exp2)) = if (exp1 == exp2) then (EMul (ENum (a+b)) (simplify exp1)) else simplifyBasic e
simplify e@(EAdd exp1 (EMul (ENum a) exp2)) = if (exp1 == exp2) then (simplifyBasic (EMul (ENum (a+1)) (simplify exp1))) else simplifyBasic e


-- Ovo na dno, specificnije cemo prvo ispitati
-- Sabiranje
simplify (EAdd exp1 exp2) = 
                          let levi = sw exp1
                              desni = sw exp2
                          in (if (levi == desni) then (simplify (EMul (ENum 2) levi)) else (simplifyBasic (EAdd levi desni)))


-- Oduzimanje
simplify (ESub exp1 exp2) = 
						  let levi = simplify exp1
						      desni = simplify exp2
						  in (if (levi == desni) then (ENum 0) else (simplifyBasic (ESub levi desni)))


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

sw x = (if (x == (simplify  $ normalize x)) then x else sw (simplify $ normalize x))