# EXPECT: (5, "a")

let f : (forall a. a -> a) -> (Number, String)
let f(g) = (g(5), g("a"))

print(f(\x -> x))


