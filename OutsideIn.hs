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
  s1 <- solves (simpleConstraints fs)
  oiprint $ "Sub1: " ++ (show s1)
  s2 <- solves (map (applySubC s1) fs)
  oiprint $ "Sub2: " ++ (show s2)
  let t' = applySub (compSub s1 s2) t
  oiprint $ "Pretype: " ++ (show t)
  oiprint $ "Type: " ++ (show t')
  return (t', fs)
 return (t, (fs, state))
