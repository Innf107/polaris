# EXPECT: 6

let f(x) = match x {
    Just(y) -> y + 1
    Nothing -> 0
}

let g : < Just(Number), Nothing > -> Number = f

print(g(Just(5)))


