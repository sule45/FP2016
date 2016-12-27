module Normalization where

import               Expression

normalize :: Exp -> Exp

normalize (EAdd exp (ENum a)) = EAdd (ENum a) $ normalize exp
normalize (EMul exp (ENum a)) = EMul (ENum a) $ normalize exp
normalize (ESub exp (ENum a)) = EAdd (ENum $ -a) $ normalize exp
normalize (EDiv exp (ENum a)) = EMul (ENum $ 1/a) $ normalize exp

normalize (EAdd exp1 exp2) = if (expWeight exp1 < expWeight exp2) then (EAdd (normalize exp1) (normalize exp2)) else (EAdd (normalize exp2) (normalize exp1))
normalize (EMul exp1 exp2) = if (expWeight exp1 < expWeight exp2) then (EMul (normalize exp1) (normalize exp2)) else (EMul (normalize exp2) (normalize exp1))

normalize exp = exp



---------------------
addWeight = 5
subWeight = 20
mulWeight = 1000
divWeight = 100000
unaryWeight = 10000000

coeff = 100000000.0

expWeight :: Exp -> Double
expWeight (ENum _) = 1
expWeight (EVar _) = 2
expWeight (EAdd exp1 exp2) = addWeight + (expWeight exp1)/coeff + (expWeight exp2)/coeff
expWeight (EMul exp1 exp2) = mulWeight + (expWeight exp1)/coeff + (expWeight exp2)/coeff
expWeight (EDiv exp1 exp2) = divWeight + (expWeight exp1)/coeff + (expWeight exp2)/coeff
expWeight (ESub exp1 exp2) = subWeight + (expWeight exp1)/coeff + (expWeight exp2)/coeff
expWeight (ENeg exp) = subWeight + (expWeight exp)/coeff
expWeight (ESin exp) = unaryWeight + (expWeight exp)/coeff
expWeight (ECos exp) = unaryWeight + (expWeight exp)/coeff + 1
expWeight (ELog exp) = unaryWeight + (expWeight exp)/coeff + 2
expWeight (EExp exp) = unaryWeight + (expWeight exp)/coeff + 3
expWeight (EPow exp1 exp2) = unaryWeight + (expWeight exp1)/coeff + (expWeight exp2)/coeff - 2
