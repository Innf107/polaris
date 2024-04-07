# KNOWN

type T(r) = < A | r >

let f : forall r. T(r) -> Number
let f(x) = match x {
    A -> 5
}

print(f(B))

