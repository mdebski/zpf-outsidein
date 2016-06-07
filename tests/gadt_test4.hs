

data T where
 MkT :: forall a. a -> (a -> Int) -> T 


foo :: T -> Int 
foo x = case x of 
          MkT (x :: a) f -> f x 

foo1 :: T -> Int 
foo1 x = case x of 
            MkT (x :: a) (f :: a -> Int) -> f x 

-- FAILS (correctly, each existential stands for itself) 
foo2 :: T -> Int 
foo2 x = case x of 
           MkT (x :: a) (f :: b -> Int) -> f x 
