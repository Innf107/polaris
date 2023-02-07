# EXPECT: 5

let f(x, y) = match (x, y) {
    (Just(a), Ok(b)) -> a + b#
    (Nothing, Err(a)) -> 5
}

print(f(Nothing, Err("a")))
