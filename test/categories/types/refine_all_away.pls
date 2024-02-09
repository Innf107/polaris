# EXPECT: 5

let f : forall r. (< | r > -> Number, < A | r >) -> Number
let f(g, x) = match x {
    A -> 5
    y -> g(y)
}

print(f(\B -> 5, B))
