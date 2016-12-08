module Derivation where

import      Expression

derive :: Exp -> Either EvalError Exp
derive (EVar s) = return $ ENum 1 
derive (EAdd exp1 exp2) = do
  v1 <- derive exp1
  v2 <- derive exp2
  return $ EAdd v1 v2
derive (ESub exp1 exp2) = do
  v1 <- derive exp1
  v2 <- derive exp2
  return $ ESub v1 v2
derive (ENeg expr) = do
	v <- derive expr
	return $ ENeg v
derive (EMul exp1 exp2) = do
  v1 <- derive exp1
  v2 <- derive exp2
  return $ EAdd (EMul v1 exp2) (EMul exp1 v2)
derive (ESin expr) = do
  v <- derive expr
  return $ EMul (ECos expr) v
derive (ECos expr) = do
  v <- derive expr
  return $ EMul (ENeg $ ESin expr) v
derive (ELog expr) = do
  v <- derive expr
  return $ EDiv v expr
derive (EExp expr) = do
	v <- derive expr
	return $ EMul (EExp expr) v
derive (EPow expr1 expr2) = do --netacno
  v2 <- derive expr2
  return $ EMul (EPow expr1 expr2) (EAdd (EMul v2 $ ELog expr1) (EDiv expr2 expr1))
derive (EDiv exp1 exp2) = do
  dividend <- derive exp1
  divisor  <- derive exp2 --mozda neka greska za deljenje se pojavi
  return $ EDiv (ESub (EMul exp1 divisor) (EMul exp2 dividend)) (EPow exp2 (ENum 2))
derive (ENum n) = return $ ENum 0