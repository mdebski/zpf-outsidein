import OutsideIn
import OIDefs

{-
data Exp t where
  Lit :: forall t. (t ~ Int) => Int -> Exp t



size :: forall a. Exp a -> Int
size e = case e of
           Lit n -> 4

size1 :: forall a. Exp a -> a
size1 e = case e of
           Lit n -> 4
-}

main = do
  outsideIn $ (LetD "Exp" [(-1)] [
    ("Lit", [], [CEq (TVar (-1)) TInt], [TInt]) ]) $
      LetA "size" (TForall [(-2)] [] $ TFun (TCons "Exp" [TVar (-2)]) TInt)
        (Lam "e" $ Case (Var "e") [(PCon "Lit" ["n"], (ILit 4))]) $
          LetA "size1" (TForall [(-2)] [] $ TFun (TCons "Exp" [TVar (-2)]) (TVar (-2)))
            (Lam "e" $ Case (Var "e") [(PCon "Lit" ["n"], (ILit 4))]) $
              (Var "size1")

