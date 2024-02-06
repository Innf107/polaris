# EXPECT: 3

let f : < A, B > -> Number
let f(x) = match x {
    A -> 1
    B -> 2
}

let g : < A, B, C > -> Number
let g(x) = match x {
    (A | B) as y -> f(y)
    C -> 3
}

print(g(C))
