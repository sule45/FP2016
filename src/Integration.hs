module Integration where

import Expression
-- po ovom spisku krenuo: https://en.wikipedia.org/wiki/Lists_of_integrals#Lists_of_integrals
-- mozda dodati promenljivu po kojoj se radi integral?
-- ostati pri Maybe?
-- dodati konstantu (kad se radi neka komplikovanija integracija, mozda zatreba)
-- fali apsolutna vrednost za integraciju
-- e^nesto se ne parsira
integrate :: Exp -> Either EvalError Exp
integrate exp = case integrate' exp of
					Nothing -> Left IntegrationFailure
					(Just exp') -> Right exp'


integrate' :: Exp -> Maybe Exp

integrate' (EVar s) = Just (EMul (ENum 0.5) (EPow (EVar s) (ENum 2)))
integrate' (ENeg exp) = case integrate' exp of
                                            Nothing -> Nothing
                                            (Just exp1) -> Just (ENeg exp1)

integrate' (ENum num) = (Just (EMul (ENum num) (EVar "x")))

integrate' (EAdd exp1 exp2) = case (integrate' exp1, integrate' exp2) of
                                             (Nothing, _) -> Nothing
                                             (_, Nothing) -> Nothing
                                             (Just exp1', Just exp2') -> (Just (EAdd exp1' exp2'))

integrate' (ESub exp1 exp2) = case (integrate' exp1, integrate' exp2) of
                                             (Nothing, _) -> Nothing
                                             (_, Nothing) -> Nothing
                                             (Just exp1', Just exp2') -> (Just (ESub exp1' exp2'))

-- 1/x
integrate' (EDiv (ENum 1) (EVar s)) = Just (ELog (EVar s)) 
integrate' (EPow (EVar s) (ENum (-1))) = Just (ELog (EVar s))  

-- mnozenje i deljenje konstantom
integrate' (EMul (ENum a) exp) = case (integrate' exp) of
                                 Nothing -> Nothing
                                 (Just exp1) -> Just (EMul (ENum a) exp1)
integrate' (EDiv exp (ENum a)) = case (integrate' exp) of
                                 Nothing -> Nothing
                                 (Just exp1) -> Just (EDiv exp1 (ENum a))

-- x^n
integrate' (EPow (EVar s) (ENum num)) = Just (EDiv (EPow (EVar s) (ENum (num + 1))) (ENum (num+1)))

-- (ax+b)^n
integrate' (EPow (EAdd (EMul (ENum a) (EVar s)) (ENum b)) (ENum n)) = Just (EDiv (EPow (EAdd (EMul (ENum a) (EVar s)) (ENum b)) (ENum (n + 1))) (EMul (ENum a) (ENum (n + 1))))

-- c/(ax+b)
integrate' (EDiv (ENum c) (EAdd (EMul (ENum a) (EVar s)) (ENum b))) = Just (EMul (ENum (c/a)) (ELog (EAdd (EMul (ENum a) (EVar s)) (ENum b))))

-- e^(ax)
integrate' (EExp (EMul (ENum a) (EVar s))) = Just (EMul (ENum (1/a)) (EExp (EMul (ENum a) (EVar s))))

-- f'(x) * e ^(f(x)) ?

-- 	a^x
integrate' (EPow (ENum a) (EVar s)) = Just (EDiv (EPow (ENum a) (EVar s)) (ENum (log a)))

-- ovo na dno
integrate' _ = Nothing