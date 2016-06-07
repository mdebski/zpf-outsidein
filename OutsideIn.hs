module OutsideIn where

import Data.List

import ConGen
import OIDefs
import Subs
import OIMonad
import Solve

type DebugInfo = ([OIConstraint], OIState)
outsideIn :: OIExpr -> IO (OIType, DebugInfo)
outsideIn e = do
 ((t, fs), state) <- runOI $ do
  oiprint $ "Expr: " ++ (show e)
  (t, fs) <- generate e
  oiprint $ "Constraints: "
  oiprint $ (intercalate "\n" (map show fs))
  s <- solves fs
  oiprint $ "Sub: " ++ (show s)
  let t' = applySub s t
  oiprint $ "Pretype: " ++ (show t)
  oiprint $ "Type: " ++ (show t')
  assertS ((fuv t') == []) "free unification variables in final type"
  return (t', fs)
 return (t, (fs, state))


