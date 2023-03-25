# EXPECT: (5, "a", 5, "a")

let f = \x -> x

let g(x) = x

print((f(5), f("a"), g(5), g("a")))
