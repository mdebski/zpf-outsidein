
data Pair a b where 
  Pair :: forall a b. a -> b -> Pair a b

data List a where 
  Nil  :: forall a. List a 
  Cons :: forall a. a -> List a -> List a 

data Maybe a where 
  Nothing :: forall a. Maybe a
  Just :: forall a. a -> Maybe a

data Equal a b where
  Eq :: forall c d. (c ~ d) => Equal c d

data T a where 
  MkT :: forall a. (a ~ Bool) => a -> T a 

data Rep a where
  RI :: forall a. (a ~ Int) => Rep a
  RP :: forall a b c. (c ~ Pair a b) => Rep a -> Rep b -> Rep c 

undefined :: forall a. a 
map :: forall a b. (a -> b) -> List a -> List b 

foo0 = Eq 

-- foo = let test :: forall a b. Rep a -> Rep b -> Equal a b
--           test = \(RP s1 t1) -> \(RP s2 t2) -> 
--                      case test s1 s2 of 
--                        Eq _ -> case test t1 t2 of 
--                                  Eq _ -> Eq
--       in 3 



add :: Int -> Int -> Int 


-- Passes, fails in original Wobbly paper, fails in Wobbly Appendix (GHC) 
double :: forall a. Rep a -> List a -> List a 
double r xs = map (\x -> case r of RI -> add x x) xs


-- Fails, passes in the original Wobbly GADT paper, fails in WA 
-- test :: forall a b. Equal a b -> Int 
-- test x = let y = (\z -> case x of Eq -> z) 
--          in 3  

-- Passes, passes in original wobbly GADT paper, fails in WA
test1 :: forall a b. Equal a b -> Int
test1 x = (\z -> case x of Eq  -> z) 34

not :: Bool -> Bool 
eq :: forall a. a -> a -> Bool 
ifThenElse :: Bool -> Bool -> Bool -> Bool 

foo :: forall a. T a -> Int 
foo y = let h x = 
           ifThenElse True 
                      (case Nil of Nil -> ifThenElse x True True)  
                      (case y of MkT z  -> eq z x)
        in 42





