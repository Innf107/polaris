# EXPECT: 5

let (f : forall a. a -> a) = \x -> x

print(f(5))
