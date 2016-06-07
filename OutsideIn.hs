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
  let (ss, ps) = splitConstraints fs
  oiprint $ "Simple constraints: "
  oiprint $ (intercalate "\n" (map show ss))
  oiprint $ "Proper constraints: "
  oiprint $ (intercalate "\n" (map show ps))
  s1 <- solves ss
  oiprint $ "Sub1: " ++ (show s1)
  let ps' = map (applySubC s1) ps
  oiprint $ "Proper constraints after s1: "
  oiprint $ (intercalate "\n" (map show ps'))
  s2 <- solves ps'
  oiprint $ "Sub2: " ++ (show s2)
  oiprint $ "Pretype: " ++ (show t)
  let t' = applySub s1 t
  oiprint $ "Type after s1: " ++ (show t')
  let t'' = applySub s2 t'
  oiprint $ "Type: " ++ (show t'')
  return (t', fs)
 return (t, (fs, state))
