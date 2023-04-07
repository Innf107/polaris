# EXPECT: 5

let f(x, y) = match (x, y) {
    (Just(a), Ok(b)) -> a + b
    (Nothing, Err(a)) -> 5
    (Just(_), Err(_)) -> 6
    (Nothing, Ok(_)) -> 7
}

print(f(Nothing, Err("a")))
