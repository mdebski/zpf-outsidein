import OutsideIn
import OIDefs

{-

data List a where
  Nil  :: forall a. List a
  Cons :: forall a. a -> List a -> List a

data T a where
   T1 :: forall a. (a ~ Bool) => Int -> T a
   T2 :: forall a. List a -> T a


null :: forall a. List a -> Bool
not :: Bool -> Bool
and :: Bool -> Bool -> Bool

f2 x = case x of
         T2 xs -> null xs
         T1 n -> False

-- Fails (correctly!)
-- f3 x = case x of
--           T1 n -> False

h2 x y = case y of
            T1 n -> and x True
             _ -> not x

-- Fails (correctly!)
--  h1 x y = case y of
--            T1 n -> and x True
--            T2 xs -> null xs

-}


defs :: OIExpr -> OIExpr
defs x = (LetD "List" [(-1)] [
    ("Nil", [], [], []) ,
    ("Cons", [], [], [TVar (-1), (TCons "List" [TVar (-1)])]) ]) $
      (LetD "T" [(-2)] [
        ("T1", [], [CEq (TVar (-2)) TBool], [TInt]),
        ("T2", [], [], [(TCons "List" [TVar (-1)])]) ]) $
          (LetA "not" (TFun TBool TBool) $ Lam "_2" $ Var "_2") $
            (LetA "and" (TFun TBool (TFun TBool TBool)) $ Lam "_3" $ Lam "_4" $ Var "_4") $
              (LetA "null" (TForall [(-3)] [] (TFun (TCons "List" [TVar (-3)]) TBool)) $ Lam "_5" $ BLit True) $
                x

f2 = do
  outsideIn $ defs $
    Let "f2" (Lam "x" $ (Case (Var "x") [
      (PCon "T2" ["xs"], (App (Var "null") (Var "xs"))),
      (PCon "T1" ["n"], (BLit False)) ])) $
        (Var "f2")


f3 = do
  outsideIn $ defs $
    Let "f3" (Lam "x" $ (Case (Var "x") [
      (PCon "T1" ["n"], (BLit False)) ])) $
        (Var "f3")

main = do
 putStrLn $ "================= F2: ==================="
 f2
 putStrLn $ "================= F3: ==================="
 f3
