# EXPECT: 5

let f : forall a. a -> a
let f(x) = (x : a)

print(f(5))
