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
 return $ undefined
generate (Var name) = do
 t <- getType name
 nt <- case t of
  (TForall vars contrs tau) -> do
    assert (contrs == [])
    fresh <- replicateM (length vars) freshMeta
    let sub = makeSub (map SVar vars) (map TMeta fresh)
    return $ applySub sub t
  _ -> return t
 return (nt, [])

generate _ = undefined
