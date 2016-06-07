-- Scoped variable test

f :: forall a. a -> Int 
f = \(x :: a) -> 3 

-- Fails (correctly!) 
-- f1 :: forall a. a -> Int 
-- f1 = \(x :: b) -> 3 