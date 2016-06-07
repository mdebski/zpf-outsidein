module Solve where

import Data.List

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
 oiprint ("Solve " ++ (show c) ++ " = " ++ (show s))
 s' <- solves $ map (applySubC s) cs
 return $ compSub s s'

solve :: OIConstraint -> OI Sub
solve c@(CEq t1 t2) = if (t1 == t2) then return emptySub else case (t1, t2) of
  ((TVar v), t) -> return $ if typeMember (SVar v) t then error $ "Infinite type in CEq TVar: " ++ (show c) else makeSub [SVar v] [t]
  ((TMeta m), t) -> return $ if typeMember (SMeta m) t then error $ "Infinite type in CEq TMeta: " ++ (show c) else makeSub [SMeta m] [t]
  (t, x@(TMeta m)) -> solve (CEq x t)
  (t, x@(TVar v)) -> solve (CEq x t)
  ((TFun t1 t2), (TFun t1' t2')) -> solves [CEq t1 t1', CEq t2 t2']
  ((TCons n ts1), (TCons m ts2)) -> do
    if n == m && (length ts1) == (length ts2)
    then solves [CEq t1 t2 | (t1,t2) <- zip ts1 ts2]
    else error $ "Unsolvable S-Cons: " ++ (show c)
  _ -> error $ "Unsolvable equality constraint: " ++ (show c)

solve c@(CImp metas tvars [] fs) = do
 s <- solves fs
 let metas' = map (applySub s) (map TMeta metas)
 let ok1 = (intersect (concatMap ftv metas') tvars) == []
 let ok2 = (intersect (map SVar tvars) (domain s)) == []
 return $ if ok1 && ok2 then s else error $ "Unsolvable S-SImpl: " ++ (show c)

solve c@(CImp metas tvars cs fs) = do
 s1 <- solves cs
 let fs' = map (applySubC s1) fs
 s2 <- solves fs'
 let ok = (intersect (map SMeta metas) (domain s2)) == []
 return $ if ok then s2 else error $ "Unsolvable S-PImpl: " ++ (show c)
