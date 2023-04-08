# EXPECT: 5

let f : (forall a. a -> a) -> Number
let f(id) = id(5)

let g : (forall b. b -> b) -> Number
let g(id) = id(6)

let h = if true then f else g

print((h(\x -> x)))