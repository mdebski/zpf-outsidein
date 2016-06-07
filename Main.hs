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
  , LetA "id" (TFun (TVar 1001) (TVar 1001)) $ Lam "_9" $ Var "_9"
  ]

expr1 :: OIExpr
expr1 = Let "f1" (Lam "x" $ Case (Var "x")
 [ (PCon "T1" ["n"], App (App (Var "cmp") (Var "n")) (ILit 0))
 ]) (ILit 42)

expr2 :: OIExpr
expr2 = Let "f2" (Lam "x" $ Case (Var "x")
 [ (PCon "T1" ["n"], App (App (Var "cmp") (Var "n")) (ILit 0))
 , (PCon "T2" ["a"], (BLit True))
 ]) (ILit 42)

expr3 :: OIExpr
expr3 = Let "h1" (Lam "x" $ Lam "y" $ Case (Var "y")
 [ (PCon "T1" ["n"], App (App (Var "and") (Var "x")) (App (App (Var "cmp") (Var "n")) (ILit 0)))
 , (PCon "T2" ["a"], (BLit True))
 ]) (ILit 42)

expr4 :: OIExpr
expr4 = Let "h2" (Lam "x" $ Lam "y" $ Case (Var "y")
 [ (PCon "T1" ["n"], App (App (Var "and") (Var "x")) (App (App (Var "cmp") (Var "n")) (ILit 0)))
 , (PCon "T2" ["a"], App (Var "not") (Var "x"))
 ]) (ILit 42)

main = do
 print expr1
 (t, (fs, state)) <- outsideIn (stdlib expr1)
 print t
 print fs
 print state
