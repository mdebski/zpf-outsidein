module OutsideIn where

import Data.List
import Control.Monad
import Control.Monad.Trans.Class

import ConGen
import OIDefs
import Subs
import OIMonad
import Solve

type DebugInfo = ([OIConstraint], OIState)
outsideIn :: OIExpr -> IO (OIType, DebugInfo)
outsideIn e = do
 ((t, fs), state) <- runOI $ do
  lift $ putStrLn $ "Expr: " ++ (show e)
  (t, fs) <- generate e
  lift $ putStrLn "Constraints: "
  lift $ putStrLn (intercalate "\n" (map show fs))
  s <- solves fs
  lift $ putStrLn "Sub: "
  lift $ print s
  let t' = applySub s t
  lift $ putStrLn $ "Type: " ++ (show t')
  return (t', fs)
 return (t, (fs, state))


