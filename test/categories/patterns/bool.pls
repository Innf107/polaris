# EXPECT: (5, 6)

let f(x) = match x {
    true -> 5
    false -> 6
}

print((f(true), f(false)))
