# EXPECT: (5, "a")

let f(g : forall a. a -> a) = (g(5), g("a"))

print(f(\x -> x))


