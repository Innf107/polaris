# EXPECT: (5, 2)

let f(x) = match x {
    { y = 1, z } -> z
    { y, z } -> y
}


print((f({ y = 1, z = 5 }), f({y = 2, z = 6})))

