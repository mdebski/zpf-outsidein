module Solve where

import OIDefs
import OIMonad
import Subs

simpleConstraints :: [OIConstraint] -> [OIConstraint]
simpleConstraints = concatMap simpleConstraint

simpleConstraint :: OIConstraint -> [OIConstraint]
simpleConstraint c@(CEq _ _) = [c]
simpleConstraint (CImp m t [] cs) = [CImp m t [] (simpleConstraints cs)]
simpleConstraint _ = []

typeMember :: SubVar -> OIType -> Bool
typeMember v (TFun t1 t2) = (typeMember v t1) || (typeMember v t2)
typeMember v (TCons _ ts) = any (typeMember v) ts
typeMember v (TForall _ _ t) = typeMember v t
typeMember (SMeta m) (TMeta m') = m == m'
typeMember (SVar v) (TVar v') = v == v'
typeMember _ _ = False

solves :: [OIConstraint] -> OI Sub
solves [] = return $ emptySub
solves (c:cs) = do
 s <- solve c
 solves $ map (applySubC s) cs

solve :: OIConstraint -> OI Sub
solve (CEq (TVar v) t) = return $ if typeMember (SVar v) t then emptySub else
 makeSub [SVar v] [t]
solve (CEq (TMeta m) t) = return $ if typeMember (SMeta m) t then emptySub else
 makeSub [SMeta m] [t]
solve (CEq t x@(TMeta m)) = solve (CEq x t)
solve (CEq t x@(TVar m)) = solve (CEq x t)
solve (CEq (TFun t1 t2) (TFun t1' t2')) = solves [CEq t1 t1', CEq t2 t2']
solve (CEq (TCons n ts1) (TCons m ts2)) = do
 assert $ n == m
 assert $ (length ts1) == (length ts2)
 solves [CEq t1 t2 | (t1,t2) <- zip ts1 ts2]
solve _ = return $ emptySub -- TODO: is that correct?
