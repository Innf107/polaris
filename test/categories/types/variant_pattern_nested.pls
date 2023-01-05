
let f(x, y) = match (x, y) {
    (Just(a), Ok(b)) -> a + b#
    (Nothing, Err(a)) -> a
}

f(Nothing, Err("a"))
