# EXPECT: 5

let f : forall a. a -> a
let f(x) = x

print(f(5))
