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
    return (applySub sub tau, applySubC sub contrs)
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
    fuv <- getFuv
    return $ [CImp fuv vars [] ([(CEq tau e1t)] ++ e1f)] ++ e2f
  _ -> return $ [CEq dect e1t] ++ e1f ++ e2f
 return (e2t, constrs)

generate _ = undefined
