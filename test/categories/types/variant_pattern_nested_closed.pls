
let f(x, y) = match (x, y) {
    (Just(a), Ok(b)) -> a + b
    (Nothing, Err(a)) -> a
    (Just(a), Err(b)) -> 5
    (Nothing, Ok(_)) -> 6

}

print(f(Nothing, C))
