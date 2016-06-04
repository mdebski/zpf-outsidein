module ConGen where

import Control.Monad

import OIDefs
import OIMonad
import Subs

generate :: OIExpr -> OI (OIType, [OIConstraint])
generate (ILit _) = return (TInt, [])
generate (BLit _) = return (TBool, [])

generate (Con name) = do
 t <- getType name
 case t of
  (TForall vars contrs tau) -> do
    fresh <- replicateM (length vars) freshMeta
    let sub = makeSub (map SVar vars) fresh
    return (applySub sub tau, map (applySubC sub) contrs)
  _ -> return (t, [])

generate (Var name) = do
 t <- getType name
 nt <- case t of
  (TForall vars contrs tau) -> do
    assert (contrs == [])
    fresh <- replicateM (length vars) freshMeta
    let sub = makeSub (map SVar vars) fresh
    return $ applySub sub tau
  _ -> return t
 return (nt, [])

generate (LetD name vars dcons e) = do
 addData name vars dcons
 generate e

generate (Lam name e) = do
 alpha <- freshMeta
 (et, econstr) <- withType name alpha (generate e)
 return (TFun alpha et, econstr)

generate (App e1 e2) = do
 (e1t, e1f) <- generate e1
 (e2t, e2f) <- generate e2
 alpha <- freshMeta
 return (alpha, [CEq e1t (TFun alpha e2t)] ++ e1f ++ e2f)

generate (LetA name dect e1 e2) = do
 (e1t, e1f) <- withType name dect $ generate e1
 (e2t, e2f) <- withType name dect $ generate e2
 constrs <- case dect of
  (TForall vars contrs tau) -> do
    assert (contrs == [])
    fuv <- getEnvFuv
    return $ [CImp fuv vars [] ([(CEq tau e1t)] ++ e1f)] ++ e2f
  _ -> return $ [CEq dect e1t] ++ e1f ++ e2f
 return (e2t, constrs)

generate (Case e epats) = do
 let (pats, es) = unzip epats
 let ts = map (\(PCon n _) -> n) pats
 assert (ts /= [])
 let t = head ts
 assert (all (\x -> x == t) ts)
 (et, ef) <- generate e
 (tvs, _) <- getTCon t
 alphas <- replicateM (length tvs) freshMeta
 beta <- freshMeta
 fis <- sequence [generatePat pi ei (tvs, alphas) beta | (pi, ei) <- epats]
 let fis' = concat fis
 return (beta, ef ++ [CEq et (TCons t alphas)] ++ fis')

generate (Let name e1 e2) = do
 undefined

generatePat :: OIPat -> OIExpr -> ([TypeVar], [OIType]) -> OIType -> OI [OIConstraint]
generatePat (PCon n ns) e (tvs, alphas) dec_et = do
 (bs, cons, ts) <- getDCon n
 -- TODO: \bar{b} \not\in ftv(gamma, dec_et)
 assert $ (length ns) == (length ts)
 let phi = makeSub (map SVar tvs) alphas
 let ts' = map (applySub phi) ts
 let cons' = map (applySubC phi) cons
 (et, ef) <- withTypes ns ts' $ generate e
 envfuv <- getEnvFuv
 let fuvs = envfuv ++ (concatMap fuv alphas) ++ (fuv dec_et)
 return $ [CImp fuvs bs cons' ((CEq et dec_et):ef)]
