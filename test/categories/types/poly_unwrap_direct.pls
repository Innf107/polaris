# EXPECT: Not sure tbqh

data T = (forall a. a -> a) -> Number

let f : () -> T
let f() = T (\f -> f(5))

let _ = { f()!(\x -> x) }


