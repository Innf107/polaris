# EXPECT: 3

let f(g) = g(1, 2)

print(f(\x y -> x + y))

