# EXPECT: 0

data A = Number


let f(x : A) = match (x!, 0) {
    _ -> 0
}

print(f(A(0)))