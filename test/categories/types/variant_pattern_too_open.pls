let f(x) = match x {
    Just(y) -> y + 1
    Nothing -> 0
}

print(f(A(5)))
