module Simplification where

import Expression
import Normalization

simplify :: Exp -> Exp

-- aritmeticke operacije i stepenovanje nad brojevima (konstantama)
simplify (EAdd (ENum x) (ENum y)) = ENum (x+y)
simplify (EMul (ENum x) (ENum y)) = ENum (x*y)
simplify (EDiv (ENum x) (ENum y)) = ENum (x/y)
simplify (ESub (ENum x) (ENum y)) = ENum (x-y)

-- 0^0 je nedefinisano
simplify exp@(EPow (ENum 0) (ENum 0)) = exp
simplify (EPow (ENum x) (ENum y)) = ENum (x**y)

--Sabiranje  s nulom 
simplify (EAdd exp (ENum 0)) = simplify exp
simplify (EAdd (ENum 0) exp) = simplify exp

-- Oduzimanje nule i od nule
simplify (ESub exp (ENum 0)) = simplify exp
simplify (ESub (ENum 0) exp) = ENeg (simplify exp)
simplify (ESub exp1 (ENeg exp2)) = EAdd (simplify exp1) (simplify exp2)

--Mnozenje nulom
simplify (EMul (ENum 0) exp) = ENum 0
simplify (EMul exp (ENum 0)) = ENum 0  

--Mnozenje i deljenje jedinicom
simplify (EMul (ENum 1) exp) = simplify exp
simplify (EMul exp (ENum 1)) = simplify exp
simplify (EDiv exp (ENum 1)) = simplify exp

simplify (EDiv (ENum 0) exp) = ENum 0

--skracivanje konstanti u izrazima (ako sabiramo sa umnoskom tog izraza, dodati jedan tom umnosku, itd) 
simplify (EAdd (ENum a) (EAdd (ENum b) exp)) = EAdd (ENum (a+b)) (simplify exp)
simplify (EAdd (EAdd (ENum a) exp1) (EAdd (ENum b) exp2)) = EAdd (ENum (a+b)) (simplify (EAdd (simplify exp1) (simplify exp2)))

simplify (EAdd (EMul (ENum a) exp1) (EMul (ENum b) exp2)) = if (exp1 == exp2) 
                                                            then EMul (ENum (a+b)) (simplify exp1)
                                                            else  (EAdd (EMul (ENum a) (simplify exp1)) (EMul (ENum b) (simplify exp2)))
simplify (EAdd exp1 (EMul (ENum a) exp2)) = if (exp1 == exp2) 
                                            then EMul (ENum (a+1)) (simplify exp1)
                                            else EAdd (simplify exp1) (EMul (ENum a) (simplify exp2))

--
simplify (EAdd (EMul (ENum a) exp1) exp2) = if (exp1 == exp2) 
                                            then EMul (ENum (a+1)) (simplify exp1)
                                            else EAdd (EMul (ENum a) (simplify exp1)) (simplify exp2)


simplify (EMul (EMul (ENum a) exp1) (EMul (ENum b) exp2)) = if (exp1 == exp2)
                                                            then EMul (ENum (a*b)) (EPow (simplify exp1) (ENum 2))
                                                            else EMul (ENum (a*b)) (EMul (simplify exp1) (simplify exp2))

simplify (EMul (ENum a) (EMul (ENum b) exp1)) = EMul (ENum (a*b)) exp1
simplify (EMul exp1 (EMul (ENum a) exp2)) = if(exp1 == exp2)
                                            then (EMul (ENum a) (EPow (simplify exp1) (ENum 2)))
                                            else (EMul (simplify exp1) (EMul (ENum a) (simplify exp2)))

-- ako imamo mnozenje izraza sa stepenom tog izraza, mozemo da pojednostavimo izraz
simplify (EMul (EPow exp1 expPow1) (EPow exp2 expPow2)) = if (exp1 == exp2)
                                                          then EPow (simplify exp1) (simplify (EAdd (simplify expPow1) (simplify expPow2)))
                                                          else EMul (EPow (simplify exp1) (simplify expPow1)) (EPow (simplify exp2) (simplify expPow2))

simplify (EMul exp1 (EPow exp2 expPow)) = if (exp1 == exp2)
                                          then (EPow (simplify exp1) (simplify (EAdd (ENum 1) expPow)))
                                          else (EMul (simplify exp1) (EPow (simplify exp2) (simplify expPow)))

-- Stepenovanje jedinicom
-- Dodati stepenovanje nulom, uz neke provere?
simplify (EPow exp (ENum 1)) = simplify exp
simplify (EPow (ENum 0) exp) = ENum 0
simplify (EPow exp (ENum 0)) = (ENum 1)

-- Neparnost sinusa i parnost kosinusa
simplify (ESin (ENeg exp)) = ENeg (ESin (simplify exp))
simplify (ECos (ENeg exp)) = ECos (simplify exp)

-- sin(x)^2 + cos(x)^2 = 1
simplify (EAdd (EPow (ESin exp1) (ENum 2)) (EPow (ECos exp2) (ENum 2))) = 
                            if (exp1 == exp2) 
                            then ENum 1
                            else EAdd (EPow (ESin (simplify exp1)) (ENum 2)) (EPow (ECos (simplify exp2)) (ENum 2))

simplify (EAdd (EPow (ECos exp1) (ENum 2)) (EPow (ESin exp2) (ENum 2))) = 
                            if (exp1 == exp2) 
                            then ENum 1 
                            else EAdd (EPow (ECos (simplify exp1)) (ENum 2)) (EPow (ESin (simplify exp2)) (ENum 2))

-- Skracivanje minusa pri deljenju i mnozenju
simplify (EDiv (ENeg exp1) (ENeg exp2)) = EDiv (simplify exp1) (simplify exp2)
simplify (EMul (ENeg exp1) (ENeg exp2)) = EMul (simplify exp1) (simplify exp2)

-- Negacija negativnog broja 
simplify (ENeg (ENum num)) = ENum (-num)
simplify (ENeg (ENeg exp)) = simplify exp

-- Minus izvlacimo ispred zagrada
simplify (EMul (ENeg exp1) exp2) = ENeg (EMul (simplify exp1) (simplify exp2))
simplify (EMul exp1 (ENeg exp2)) = ENeg (EMul (simplify exp1) (simplify exp2))
simplify (EAdd (ENeg exp1) (ENeg exp2)) = ENeg (EAdd  (simplify exp1) (simplify exp2))

--simplify (ESub (ENeg exp1) (ENeg exp2)) = (ENeg (ESub exp1 exp2))
simplify (ESub (ENeg exp1) exp2) = ENeg (EAdd (simplify exp1) (simplify exp2))

-- ako se negacija nadje u zbiru, pretvaramo to u razliku
simplify (EAdd exp1 (ENeg exp2)) = ESub (simplify exp1) (simplify exp2)
simplify (EAdd (ENeg exp1) exp2) = ESub (simplify exp2) (simplify exp1)

-- Ovo moze i ne mora; (ako je argument sinusa/kosinusa numericka vrednost van [-pi, pi], ubaciti u dati interval)
simplify exp@(ESin (ENum a)) = if (a > pi) 
                               then (simplify (ESin (ENum (a-pi)))) 
                               else (if (a < -pi) 
                                     then (simplify (ESin (ENum (a+pi)))) 
                                     else exp)

simplify exp@(ECos (ENum a)) = if (a > pi) 
                               then (simplify (ECos (ENum (a-pi)))) 
                               else (if (a < -pi) 
                                     then (simplify (ECos (ENum (a+pi)))) 
                                     else exp)

-- sabiranje i oduzimanje s istim deliocem
simplify (EAdd (EDiv exp1 exp2) (EDiv exp3 exp4)) = if (exp2 == exp4) 
                                                    then (EDiv (simplify (EAdd (simplify exp1) (simplify exp3))) (simplify exp4))
                                                    else (EAdd (EDiv (simplify exp1) (simplify exp2)) (EDiv (simplify exp3) (simplify exp4)))
simplify (ESub (EDiv exp1 exp2) (EDiv exp3 exp4)) = if (exp2 == exp4) 
                                                    then (EDiv (simplify (ESub (simplify exp1) (simplify exp3))) (simplify exp4))
                                                    else (ESub (EDiv (simplify exp1) (simplify exp2)) (EDiv (simplify exp3) (simplify exp4)))

-- log(a^b) = b*log(a)
simplify (ELog (EPow exp1 exp2)) = (EMul (simplify exp2) (ELog (simplify exp1)))

-- (x^y)^z = x^(y*z)
simplify(EPow (EPow exp1 exp2) exp3) = EPow (simplify exp1) (simplify (EMul (simplify exp2) (simplify exp3)))
--------------------------------------------
-- Ovo na dno, specificnije prvo ispitati --
--------------------------------------------

-- Sabiranje
simplify (EAdd exp1 exp2) = 
                          let levi = simplify exp1
                              desni = simplify exp2
                          in 
                            if (levi == desni) 
                            then EMul (ENum 2) levi
                            else EAdd levi desni

-- Oduzimanje
simplify (ESub exp1 exp2) = 
						  let levi = simplify exp1
						      desni = simplify exp2
						  in 
              if (levi == desni) 
                then (ENum 0) 
                else (ESub levi desni)

-- Mnozenje
simplify exp@(EMul exp1 exp2) = 
                          let levi = simplify exp1
                              desni = simplify exp2
                          in if (levi == desni)
                             then EPow levi (ENum 2)
                             else  EMul levi desni

-- Deljenje
-- f(x)/f(x) se ponasa kao 1 (sem u nulama funkcije f)
simplify (EDiv exp1 exp2) = 
            let e1 = simplify exp1
                e2 = simplify exp2
            in
              if (e1 == e2) then (ENum 1)
              else (EDiv e1 e2)

-- ostali slucajevi
simplify (ENeg exp) = ENeg  (simplify exp)
simplify (EPow exp1 exp2) = EPow (simplify exp1) (simplify exp2)
simplify (ESin exp) = ESin (simplify exp)
simplify (ECos exp) = ECos (simplify exp) 
simplify (ELog exp) = ELog (simplify exp)
simplify (EExp exp) = EExp (simplify exp)

-- Ako se ne uklapa u prethodne sablone
simplify exp = exp

-- Pokusavamo da pojednostavimo izraz sve dok se desava neka promena
simplifyConverge x = 
  let simplified = simplify $ normalize x
        in 
        if (simplified == x) then x
        else  simplifyConverge simplified