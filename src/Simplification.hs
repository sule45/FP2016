module Simplification where

import Expression
import Normalization

simplifyBasic :: Exp -> Exp

-- aritmeticke operacije i stepenovanje nad brojevima (konstantama)
simplifyBasic (EAdd (ENum x) (ENum y)) = ENum (x+y)
simplifyBasic (EMul (ENum x) (ENum y)) = ENum (x*y)
simplifyBasic (EDiv (ENum x) (ENum y)) = ENum (x/y)
simplifyBasic (ESub (ENum x) (ENum y)) = ENum (x-y)
simplifyBasic (EPow (ENum x) (ENum y)) = ENum (x**y)

--Sabiranje  s nulom 
simplifyBasic (EAdd exp (ENum 0)) = simplify exp
simplifyBasic (EAdd (ENum 0) exp) = simplify exp

-- Oduzimanje nule i od nule
simplifyBasic (ESub exp (ENum 0)) = simplify exp
simplifyBasic (ESub (ENum 0) exp) = ENeg (simplify exp)
simplifyBasic (ESub exp1 (ENeg exp2)) = EAdd (simplify exp1) (simplify exp2)

--Mnozenje nulom
simplifyBasic (EMul (ENum 0) exp) = ENum 0
simplifyBasic (EMul exp (ENum 0)) = ENum 0

--Mnozenje i deljenje jedinicom
simplifyBasic (EMul (ENum 1) exp) = simplify exp
simplifyBasic (EMul exp (ENum 1)) = simplify exp
simplifyBasic (EDiv exp (ENum 1)) = simplify exp


-- Stepenovanje jedinicom
-- Dodati stepenovanje nulom, uz neke provere?
simplifyBasic (EPow exp (ENum 1)) = simplify exp

-- Neparnost sinusa i parnost kosinusa
simplifyBasic (ESin (ENeg exp)) = ENeg (ESin (simplify exp))
simplifyBasic (ECos (ENeg exp)) = ECos (simplify exp)

-- sin(x)^2 + cos(x)^2 = 1
-- Ovde moramo proveriti da li su exp1 i exp2 jednaki bolje; o tom potom
simplifyBasic (EAdd (EPow (ESin exp1) (ENum 2)) (EPow (ECos exp2) (ENum 2))) = 
                            if ((normalize exp1) == (normalize exp2)) then (ENum 1) 
                            else (EAdd (EPow (ESin exp1) (ENum 2)) (EPow (ECos exp2) (ENum 2)))
simplifyBasic (EAdd (EPow (ECos exp1) (ENum 2)) (EPow (ESin exp2) (ENum 2))) = 
                            if ((normalize exp1) == (normalize exp2)) then (ENum 1) 
                            else (EAdd (EPow (ECos exp1) (ENum 2)) (EPow (ESin exp2) (ENum 2)))

-- Skracivanje minusa pri deljenju i mnozenju
simplifyBasic (EDiv (ENeg exp1) (ENeg exp2)) = (EDiv (simplify exp1) (simplify exp2))
simplifyBasic (EMul (ENeg exp1) (ENeg exp2)) = (EMul (simplify exp1) (simplify exp2))

-- f(x)/f(x) se ponasa kao 1 (sem u nulama funkcije f)
simplifyBasic (EDiv exp1 exp2) = 
            if ((sw exp1) == (sw exp2)) then (ENum 1)
            else (EDiv (sw exp1) (sw exp2))

-- Negacija negativnog broja ; razmisliti da li ovo zadrzavamo
simplifyBasic (ENeg (ENum num)) = ENum (-num)
simplifyBasic (ENeg (ENeg exp)) = simplify exp

-- Minus izvlacimo ispred zagrada
simplifyBasic (EMul (ENeg exp1) exp2) = (ENeg (EMul (simplify exp1) (simplify exp2)))
simplifyBasic (EMul exp1 (ENeg exp2)) = (ENeg (EMul (simplify exp1) (simplify exp2)))

simplifyBasic (EAdd (ENeg exp1) (ENeg exp2)) = (ENeg (EAdd  (simplify exp1) (simplify exp2)))
-- (-a-(-b)) = -(a-b) ; koristiti ovo?
--simplifyBasic (ESub (ENeg exp1) (ENeg exp2)) = (ENeg (ESub exp1 exp2))
-- ako se negacija nadje u zbiru, pretvaramo to u razliku
simplifyBasic (EAdd exp1 (ENeg exp2)) = ESub (simplify exp1) (simplify exp2)
simplifyBasic (EAdd (ENeg exp1) exp2) = ESub (simplify exp2) (simplify exp1)

-- Ako se ne uklapa u prethodne sablone
simplifyBasic exp =  exp

-- razlog zasto sam napravio simplify i simplifyBasic je da ne bi dolazilo do beskonacne rekurzije
simplify :: Exp -> Exp

--skracivanje konstanti u izrazima (ako sabiramo sa umnoskom tog izraza, dodati jedan tom umnosku)
simplify (EAdd (ENum a) (EAdd (ENum b) exp)) = simplifyBasic $ EAdd (ENum (a+b)) (sw exp)
simplify (EAdd (EAdd (ENum a) exp1) (EAdd (ENum b) exp2)) = simplifyBasic (EAdd (ENum (a+b)) (sw (EAdd (sw exp1) (sw exp2))))

simplify e@(EAdd (EMul (ENum a) exp1) (EMul (ENum b) exp2)) = if (exp1 == exp2) then (EMul (ENum (a+b)) (sw exp1)) else simplifyBasic e
simplify e@(EAdd exp1 (EMul (ENum a) exp2)) = if (exp1 == exp2) then (simplifyBasic (EMul (ENum (a+1)) (sw exp1))) else simplifyBasic e



-- Ovo na dno, specificnije cemo prvo ispitati
-- Sabiranje
simplify (EAdd exp1 exp2) = 
                          let levi = sw exp1
                              desni = sw exp2
                          in (if (levi == desni) then (simplify (EMul (ENum 2) levi)) else (simplifyBasic (EAdd levi desni)))


-- Oduzimanje
simplify (ESub exp1 exp2) = 
						  let levi = sw exp1
						      desni = sw exp2
						  in (if (levi == desni) then (ENum 0) else (simplifyBasic (ESub levi desni)))


-- Mnozenje
simplify (EMul exp1 exp2) = 
                          let levi = simplify exp1
                              desni = simplify exp2
                          in  simplifyBasic (EMul levi desni)

--Deljenje
simplify (EDiv exp1 exp2) = 
                          let levi = sw exp1
                              desni = sw exp2
                          in  simplifyBasic (EDiv levi desni)

-- Negacija
simplify (ENeg exp) = simplifyBasic (ENeg $ sw exp)


-- Ovo moze i ne mora (ako je argument sinusa/kosinusa numericka vrednost van [-pi, pi], ubaciti u dati interval)
simplify exp@(ESin (ENum a)) = if (a > pi) then (simplify (ESin (ENum (a-pi)))) else
                                                                          (if (a < -pi) then (simplify (ESin (ENum (a+pi)))) else exp)

simplify exp@(ECos (ENum a)) = if (a > pi) then (simplify (ECos (ENum (a-pi)))) else
                                                                          (if (a < -pi) then (simplify (ECos (ENum (a+pi)))) else exp)

-- ostali slucajevi
simplify (EPow exp1 exp2) = simplifyBasic (EPow (sw exp1) (sw exp2))
simplify (ESin exp) = simplifyBasic (ESin (sw exp))
simplify (ECos exp) = simplifyBasic (ECos (sw exp))
--ova dva nikad 
simplify (ELog exp) = simplifyBasic (ELog (sw exp))
simplify (EExp exp) = simplifyBasic (EExp (sw exp))

-- Ako se ne uklapa u prethodne sablone (da li je neophodno?)
simplify exp = simplifyBasic exp

sw x = (if (x == (simplify $ normalize x)) then x else sw (simplify $ normalize x))