# EXPECT: 1

let f(x) = match x {
    Just(y) -> y + 1
    Nothing -> 0
    _ -> 1
}

let g : forall r. < Just(Number), Nothing | r > -> Number = f

print(g(A(5)))
