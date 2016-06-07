import OutsideIn
import OIDefs

{-
data List a where
  Nil  :: forall a. List a
  Cons :: forall a. Int -> List a -> List a


head :: forall a. List a -> Int
head lst = case lst of
             Cons x y -> x

tail lst = case lst of
             Cons x y -> y
-}

main = do
  outsideIn $ (LetD "List" [(-1)] [
    ("Nil", [], [], []) ,
    ("Cons", [], [], [TInt, (TCons "List" [TVar (-1)])]) ]) $
      LetA "head" (TForall [(-2)] [] $ TFun (TCons "List" [TVar (-2)]) TInt)
        (Lam "lst" $ Case (Var "lst") [(PCon "Cons" ["x", "y"], Var ("x"))]) $
          Let "tail" (Lam "lst" $ Case (Var "lst") [(PCon "Cons" ["x'", "y'"], Var ("y'"))]) $
            (Var "tail")

