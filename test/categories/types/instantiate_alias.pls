# EXPECT: ("a", 5)

type F = forall a. a -> a

let f : F
let f(x) = x

let g : F = \x -> x

print((g("a"), g(5)))
