# EXPECT: 5

let f : forall a. a -> a = \(x : a) -> x

print(f(5))
