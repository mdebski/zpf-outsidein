import OIDefs
import OIMonad
import ConGen
import Solve
import OutsideIn
import Control.Exception
import Control.Monad

stdlib :: OIExpr -> OIExpr
stdlib = foldl (.) id
  -- Some types and standard functions, without real implementations, but with correct types.
  [ LetD "T" [(-1)] [
     ("T1", [], [CEq (TVar (-1)) TBool], [TInt]),
     ("T2", [], [], [TVar (-1)])
    ]
  , LetD "Pair" [(-2), (-3)] [
     ("MkP", [], [], [TVar (-2), TVar (-3)])
    ]
  , LetD "S" [] [
     ("MkS", [(-4), (-5)], [CEq (TVar (-4)) (TVar (-5))], [TVar (-4), TVar (-5)])
    ]
  , LetA "neg" (TFun TInt TInt) $ Lam "_1" $ Var "_1"
  , LetA "not" (TFun TBool TBool) $ Lam "_2" $ Var "_2"
  , LetA "and" (TFun TBool (TFun TBool TBool)) $ Lam "_3" $ Lam "_4" $ Var "_4"
  , LetA "plus" (TFun TInt (TFun TInt TInt)) $ Lam "_5" $ Lam "_6" $ Var "_5"
  , LetA "cmp" (TFun TInt (TFun TInt TBool)) $ Lam "_7" $ Lam "_8" $ BLit True
  , LetA "id" (TForall [(-11)] [] (TFun (TVar (-11)) (TVar (-11)))) $ Lam "_9" $ Var "_9"
  , LetA "cmpa" (TForall [(-12)] [] (TFun (TVar (-12)) (TFun (TVar (-12)) TBool))) $ Lam "_10" $ Lam "_11" $ BLit False
  ]

apps :: OIExpr -> [OIExpr] -> OIExpr
-- apply function to subsequent arguments
apps f [] = f
apps f (e:es) = apps (App f e) es

mkp = apps (Con "MkP")

expr0 :: OIExpr
-- let id2 = \x -> x in id (id id2)
expr0 = Let "id2" (Lam "x" (Var "x")) (App (App (Var "id") (Var "id")) (Var "id2"))

expr1 :: OIExpr
-- let g = \x -> x && True in g
expr1 = Let "g" ((Lam "x") (App (App (Var "and") (Var "x")) (BLit True))) (Var "g")

expr2 :: OIExpr
expr2 = mkp [ILit 3, BLit True]

expr3 :: OIExpr
expr3 = mkp [(Var "id"), (BLit True)]

expr4 :: OIExpr
expr4 = apps (Var "cmp") [ILit 1, ILit 2]

expr5 :: OIExpr
expr5 = mkp [mkp [ILit 3, mkp [expr4,  ILit 4]], (Var "id")]

expr01 :: OIExpr
expr01 = apps (Con "MkP") [expr0, expr1]

gadt_expr1_simp :: OIExpr
gadt_expr1_simp = (Lam "x" $ Case (Var "x")
 [ (PCon "T1" ["n"], App (App (Var "cmp") (Var "n")) (ILit 0)) ])
gadt_expr1 :: OIExpr
gadt_expr1 = Let "f1" gadt_expr1_simp (Var "f1")

gadt_expr2_simp :: OIExpr
gadt_expr2_simp = Lam "x" $ Case (Var "x")
 [ (PCon "T1" ["n"], App (App (Var "cmp") (Var "n")) (ILit 0))
 , (PCon "T2" ["a"], (BLit True))
 ]
gadt_expr2 :: OIExpr
gadt_expr2 = Let "f2" gadt_expr2_simp (Var "f2")

gadt_expr3 :: OIExpr
gadt_expr3 = Let "h1" (Lam "x" $ Lam "y" $ Case (Var "y")
 [ (PCon "T1" ["n"], App (App (Var "and") (Var "x")) (App (App (Var "cmp") (Var "n")) (ILit 0)))
 , (PCon "T2" ["a"], (BLit True))
 ]) (Var "h1")

gadt_expr4 :: OIExpr
gadt_expr4 = Let "h2" (Lam "x" $ Lam "y" $ Case (Var "y")
 [ (PCon "T1" ["n"], App (App (Var "and") (Var "x")) (App (App (Var "cmp") (Var "n")) (ILit 0)))
 , (PCon "T2" ["a"], App (Var "not") (Var "x"))
 ]) (Var "h2")

main = do
 putStrLn "==================== simple ==================="
 outsideIn (stdlib expr01)
 putStrLn "==================== f1 (FAILS) ==================="
 catch (void $ outsideIn (stdlib gadt_expr1)) (\e -> putStrLn $ "FAILED: " ++ (show (e::SomeException)))
 putStrLn "==================== f2 ==================="
 outsideIn (stdlib gadt_expr2)
 putStrLn "==================== h1 (FAILS) ==================="
 catch (void $ outsideIn (stdlib gadt_expr3)) (\e -> putStrLn $ "FAILED: " ++ (show (e::SomeException)))
 putStrLn "==================== h2 ==================="
 outsideIn (stdlib gadt_expr4)
