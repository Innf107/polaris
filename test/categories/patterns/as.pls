# EXPECT: [(1, 2)]

let f(x) = match x {
    ([(1, 2)] as a, []) as b -> a
    _ -> [(3, 4)]
}

print(f(([(1, 2)], [])))
