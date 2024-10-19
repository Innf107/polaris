# EXPECT: { w = 3 }

let f(x) = match x {
    { y, z | r } -> r
}


print(f({ y = 1, z = 2, w = 3 }))

