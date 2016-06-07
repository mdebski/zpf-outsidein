module OutsideIn where

import ConGen
import OIDefs
import Subs
import OIMonad
import Solve

type DebugInfo = ([OIConstraint], OIState)
outsideIn :: OIExpr -> IO (OIType, DebugInfo)
outsideIn e = do
 ((t, fs), state) <- runOI $ do
  (t, fs) <- generate e
  s <- solves fs
  let t' = applySub s t
  return (t', fs)
 return (t, (fs, state))


