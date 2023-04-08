# EXPECT: (5, "a")

let id(x) = x

let f(g : forall a. a -> a) = (g(5), g("a"))

print(f(id))


