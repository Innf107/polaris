# EXPECT: (5, 6)

let f(x) = match x {
    "test" -> 5
    _ -> 6
}

print((f("test"), f("a")))