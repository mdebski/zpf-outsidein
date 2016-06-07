import OIDefs
import OIMonad
import ConGen
import Solve
import OutsideIn

stdlib :: OIExpr -> OIExpr
stdlib = foldl (.) id
  -- Some types and standard functions, without real implementations, but with correct types.
  [ LetD "T" [(-1)] [
     ("T1", [], [CEq (TVar (-1)) TBool], [TInt]),
     ("T2", [], [], [TVar (-1)])
    ]
  , LetA "neg" (TFun TInt TInt) $ Lam "_1" $ Var "_1"
  , LetA "not" (TFun TBool TBool) $ Lam "_2" $ Var "_2"
  , LetA "and" (TFun TBool (TFun TBool TBool)) $ Lam "_3" $ Lam "_4" $ Var "_4"
  , LetA "plus" (TFun TInt (TFun TInt TInt)) $ Lam "_5" $ Lam "_6" $ Var "_5"
  , LetA "cmp" (TFun TInt (TFun TInt TBool)) $ Lam "_7" $ Lam "_8" $ BLit True
  , LetA "id" (TForall [1001] [] (TFun (TVar 1001) (TVar 1001))) $ Lam "_9" $ Var "_9"
  ]

expr0 :: OIExpr
-- let id2 = \x -> x in id (id id2)
expr0 = Let "id2" (Lam "x" (Var "x")) (App (App (Var "id") (Var "id")) (Var "id2"))

expr1 :: OIExpr
-- let g = \x -> x && True in g
expr1 = Let "g" ((Lam "x") (App (App (Var "and") (Var "x")) (BLit True))) (Var "g")

gadt_expr1 :: OIExpr
gadt_expr1 = Let "f1" (Lam "x" $ Case (Var "x")
 [ (PCon "T1" ["n"], App (App (Var "cmp") (Var "n")) (ILit 0))
 ]) (ILit 42)

gadt_expr2 :: OIExpr
gadt_expr2 = Let "f2" (Lam "x" $ Case (Var "x")
 [ (PCon "T1" ["n"], App (App (Var "cmp") (Var "n")) (ILit 0))
 , (PCon "T2" ["a"], (BLit True))
 ]) (ILit 42)

gadt_expr3 :: OIExpr
gadt_expr3 = Let "h1" (Lam "x" $ Lam "y" $ Case (Var "y")
 [ (PCon "T1" ["n"], App (App (Var "and") (Var "x")) (App (App (Var "cmp") (Var "n")) (ILit 0)))
 , (PCon "T2" ["a"], (BLit True))
 ]) (ILit 42)

gadt_expr4 :: OIExpr
gadt_expr4 = Let "h2" (Lam "x" $ Lam "y" $ Case (Var "y")
 [ (PCon "T1" ["n"], App (App (Var "and") (Var "x")) (App (App (Var "cmp") (Var "n")) (ILit 0)))
 , (PCon "T2" ["a"], App (Var "not") (Var "x"))
 ]) (ILit 42)

main = do
 outsideIn (stdlib expr1)
