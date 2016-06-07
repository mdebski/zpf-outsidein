
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
