
let f(x, y) = match (x, y) {
    (Just(a), Ok(b)) -> a + b
    (Nothing, Err(a)) -> a
}

print(f(Nothing, C))
